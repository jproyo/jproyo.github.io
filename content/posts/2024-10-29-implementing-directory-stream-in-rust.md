+++
title = "Part 1: Implementing a Directory Entry Stream in Rust"
date = 2024-10-29

[taxonomies]
tags = ["rust", "async", "stream", "filesystem", "tutorial"]

+++ 

[Part 2](@/posts/2024-11-05-implementing-directory-stream-in-rust-part-2.md)

# Part 1: Understanding a Custom Directory Entry Stream in Rust

Today, let's dive into an interesting implementation of `Stream` for directory entry iteration in Rust. This implementation showcases several advanced Rust concepts including async/await, state machines, and safe handling of file I/O.

> **SPOILER ALERT**: This implementation will contain a naive implementaion of the stream directory reader, which is not performant because it will create multiple futures. Go to Part 2 to see a more robust implementation.

## The Core Structure

The implementation centers around `DirEntriesIter`, which provides streaming access to directory entries in a custom filesystem format. Here's the key structure:

```rust
pub struct DirEntriesIter<'a> {
    reader: BufReader<File>,
    position: u64,
    state: State,
    // ... other fields
}
```

## State Machine Pattern

One of the most interesting aspects is the use of an explicit state machine through an enum:

```rust
enum State {
    ReadPosition,
    ReadFlag,
    ReadEntryName,
    ReadContent,
    ReadPath,
    Eof,
}
```

This state machine elegantly handles the sequential reading of directory entries, where each entry consists of:
1. A flag indicating entry type
2. A null-terminated entry name
3. Either a content hash (32 bytes) or a symlink path

## Deep Dive: Implementing a Stream for Directory Entry Headers

### Understanding the Context

Before diving into the implementation, let's understand what we're working with. We're reading a directory header file that acts as an index - it contains metadata and pointers (hashes) to the actual content files. Each entry in this header file represents:

1. A flag indicating the entry type
2. The entry name as a null-terminated string
3. A 32-byte hash that points to another file containing the actual content

This structure allows for efficient directory traversal without loading all file contents into memory.

### Entry Structure

```rust 
pub struct Entry<'a> {
    name: &'a str,
    content_file_hash: &'a [u8; 32]
}
```

This simplified structure focuses on two key pieces of information:
- `name`: A string slice representing the entry name
- `content_file_hash`: A 32-byte array slice that contains the hash pointing to the content file

### Stream Implementation

Here's the adapted Stream implementation with detailed explanations:

```rust
impl<'a> Stream for DirEntriesIter<'a> {
    type Item = Result<Entry<'a>, errors::ReadError>;
    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = self.as_mut().get_mut();
        loop {
            match this.state {
                State::Eof => {
                    // End of file reached - terminate stream
                    return Poll::Ready(None);
                },
                State::ReadPosition => {
                        // Reset buffers for new entry
                    this.entry_name.clear();
                    this.current_buffer_content = [0u8; 32];
                    // Seek to the next entry position
                    let future = this.reader.seek(tokio::io::SeekFrom::Start(this.position));
                    let mut future_mut = std::pin::pin!(future);
                    match future_mut.as_mut().poll(cx) {
                        Poll::Ready(Ok()) => {
                            this.state = State::ReadFlag;
                        },
                        Poll::Ready(Err(e)) => {
                            return Poll::Ready(Some(Err(errors::ReadError::from(e))));
                        },
                        Poll::Pending => return Poll::Pending,
                    }
                },
                State::ReadFlag => {
                    // Read the entry type flag
                    let future = this.reader.read_u8();
                    let mut future_mut = std::pin::pin!(future);
                    match future_mut.as_mut().poll(cx) {
                        Poll::Ready(Ok(flag)) => {
                            this.flag = flag;
                            this.state = State::ReadEntryName;
                        },
                        Poll::Ready(Err(e)) => {
                            return Poll::Ready(Some(Err(errors::ReadError::from(e))));
                        },
                        Poll::Pending => return Poll::Pending,
                    }
                },
                State::ReadEntryName => {
                    // Read until null terminator
                    let future = this.reader.read_until(0x00, &mut this.entry_name);
                    let mut future_mut = std::pin::pin!(future);
                    match future_mut.as_mut().poll(cx) {
                        Poll::Ready(Ok()) => {
                            // Remove null terminator
                            this.entry_name.pop();
                            this.state = State::ReadContent;
                        },
                        Poll::Ready(Err(e)) => {
                            return Poll::Ready(Some(Err(errors::ReadError::from(e))));
                        },
                        Poll::Pending => return Poll::Pending,
                    }
                },
                State::ReadContent => {
                    // Read the content file hash
                    let future = this.reader.read_exact(&mut this.current_buffer_content);
                    let mut future_mut = std::pin::pin!(future);
                    match future_mut.as_mut().poll(cx) {
                        Poll::Ready(Ok()) => {
                            // Update position for next entry
                            this.position += this.entry_name.len() as u64 + 33; // name + null + hash
                            // Convert name bytes to str
                            let name = std::str::from_utf8(&this.entry_name)
                                .map_err(|e| errors::ReadError::InvalidUtf8(e))?;
                            // Create static references
                            let static_name: &'static str = Box::leak(name.to_string().into_boxed_str());
                            let static_hash: &'static [u8; 32] = Box::leak(Box::new(this.current_buffer_content));
                            this.state = State::ReadPosition;
                            return Poll::Ready(Some(Ok(Entry {
                                name: static_name,
                                content_file_hash: static_hash,
                            })));
                        },
                        Poll::Ready(Err(e)) => {
                            return Poll::Ready(Some(Err(errors::ReadError::from(e))));
                        },
                        Poll::Pending => return Poll::Pending,
                    }
                }
            }
        }
    }
}
```

## Why a State Machine?

The state machine pattern is crucial here for several reasons:

1. **Async Operation Handling**: Each read operation is asynchronous and might not complete immediately. The state machine allows us to:
   - Resume from the correct position when an operation returns `Poll::Pending`
   - Maintain context between async operations
   - Handle partial reads correctly

2. **Sequential Operations**: Reading an entry requires multiple steps that must happen in order:
   - Seek to position
   - Read flag
   - Read name
   - Read content hash
   The state machine ensures these operations happen in sequence while maintaining async compatibility.

3. **Resource Management**: The state machine helps manage buffers and intermediate state between async operations, ensuring we don't lose data between polls.

## The Header File Structure

The file we're reading is a directory header file, which serves as an index or map for the actual content files. Here's how it works:

1. **Header Entry Structure**:
   ```
   [1 byte flag][variable-length name + null][32 byte content hash]
   ```

2. **Content Hash Purpose**: The `content_file_hash` field is a 32-byte hash that:
   - Acts as a unique identifier for the content file
   - Can be used to locate the actual content file in the filesystem
   - Provides integrity verification for the content

3. **Benefits of This Approach**:
   - Directory operations are fast since they only read metadata
   - Content files can be deduplicated (same content = same hash)
   - Content can be loaded on-demand rather than all at once
   - Directory structure remains compact

This separation of concerns between directory structure and content storage is similar to how modern filesystems handle inodes and data blocks, but using content-addressable storage based on hashes.

## Trade-offs and Considerations

While this implementation works, the main trade-offs that developers should be aware is that when we are returning `Poll::Pending`, the next call to `poll_next` will again create a future if the previous future has not been complete. This could lead to Memory leaks and performance issue if the file is too big.

We will treat a technique to solve this issue in the [second part](@/posts/2024-11-05-implementing-directory-stream-in-rust-part-2.md) of this serie.

## Conclusion

This implementation showcases several important patterns and considerations in modern Rust systems programming:


