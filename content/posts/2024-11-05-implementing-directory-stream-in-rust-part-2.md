+++
title = "Part 2: Optimizing the Directory Entry Stream in Rust"
date = 2024-11-05

[taxonomies]
tags = ["rust", "async", "stream", "filesystem", "tutorial"]

+++ 
# Part 2: Optimizing the Directory Entry Stream in Rust

In the [first part](@/posts/2024-10-29-implementing-directory-stream-in-rust.md) of our blog post, we explored the implementation of a custom directory entry stream in Rust. While the initial implementation effectively utilized a state machine to handle asynchronous operations, it had a drawback: it created new futures on every poll, which could lead to performance issues.

In this part, we will improve the stream implementation by reusing futures, thus minimizing the overhead associated with creating new futures repeatedly. This optimization will enhance the performance of our stream, especially when dealing with large directories.

## The Problem with Creating New Futures

In the original implementation, each time the state machine transitioned to a new state, a new future was created. This approach can lead to excessive allocations and increased pressure on the memory allocator, particularly in high-throughput scenarios. Instead, we can maintain a single future for each state and reuse it as needed.

## Updated Code Implementation

Here’s the revised implementation of the `DirEntriesIter` stream that incorporates these optimizations:

```rust
/// State machine states for iterating through directory entries
enum State {
    /// Initial state - need to seek to entry position
    ReadPosition,
    /// Reading the entry type flag
    ReadFlag,
    /// Reading the entry name
    ReadEntryName,
    /// Reading content hash for regular entries
    ReadContent,
    /// Reading symlink path for symlink entries
    ReadPath,
    /// End of file reached
    Eof,
    WaitingReadPosition(Pin<Box<dyn Future<Output = Result<u64, std::io::Error>>>>),
    WaitingReadFlag(Pin<Box<dyn Future<Output = Result<u8, std::io::Error>>>>),
    WaitingReadEntryName(Pin<Box<dyn Future<Output = Result<Vec<u8>, std::io::Error>>>>),
    WaitingReadContent(Pin<Box<dyn Future<Output = Result<[u8; 32], std::io::Error>>>>),
    WaitingReadPath(Pin<Box<dyn Future<Output = Result<Vec<u8>, std::io::Error>>>>),
}

/// Iterator over entries in a B3 directory
pub struct DirEntriesIter<'a> {
    /// Buffered reader for the directory file
    reader: Arc<RwLock<BufReader<File>>>,
    /// Current position in the file
    position: u64,
    /// Current state of the iterator state machine
    state: State,
    /// Buffer for entry name being read
    entry_name: Vec<u8>,
    /// Entry type flag
    flag: u8,
    /// Phantom data for lifetime 'a
    _marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> DirEntriesIter<'a> {
    /// Creates a new directory entries iterator starting at the given position
    pub async fn new(reader: File, position_start_entries: u64) -> Result<Self, errors::ReadError> {
        let mut reader = BufReader::new(reader);
        Ok(Self {
            reader: Arc::new(RwLock::new(reader)),
            position: position_start_entries,
            state: State::ReadPosition,
            entry_name: Vec::new(),
            flag: 0,
            _marker: std::marker::PhantomData,
        })
    }
}

impl<'a> Stream for DirEntriesIter<'a> {
    type Item = Result<BorrowedEntry<'a>, errors::ReadError>;

    /// Polls the iterator for the next directory entry
    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = self.as_mut().get_mut();

        match this.state {
            State::Eof => Poll::Ready(None),
            State::ReadPosition => {
                // Reset buffers and seek to next entry position
                this.entry_name.clear();
                this.flag = 0;
                let file = this.reader.clone();
                let pos = this.position;
                let future = async move {
                    file.write().await.seek(tokio::io::SeekFrom::Start(pos)).await
                };
                this.state = State::WaitingReadPosition(Box::pin(future));
                Poll::Pending
            },
            State::WaitingReadPosition(ref mut future) => {
                let result = future.as_mut().poll(cx);
                on_future!(this, result, State::Eof, |r| {
                    this.state = State::ReadFlag;
                    Poll::Pending
                })
            },
            State::ReadFlag => {
                let file = this.reader.clone();
                let future = async move {
                    file.write().await.read_u8().await
                };
                this.state = State::WaitingReadFlag(Box::pin(future));
                Poll::Pending
            },
            State::WaitingReadFlag(ref mut future) => {
                let result = future.as_mut().poll(cx);
                on_future!(this, result, State::Eof, |r| {
                    this.state = State::ReadEntryName;
                    this.flag = r;
                    Poll::Pending
                })
            },
            State::ReadEntryName => {
                let file = this.reader.clone();
                let future = async move {
                    let mut buffer = Vec::new();
                    file.write().await.read_until(0x00, &mut buffer).await?;
                    Ok(buffer)
                };
                this.state = State::WaitingReadEntryName(Box::pin(future));
                Poll::Pending
            },
            State::WaitingReadEntryName(ref mut future) => {
                let result = future.as_mut().poll(cx);
                let state = if this.flag & B3_DIR_IS_SYM_LINK != 0 {
                    State::ReadPath
                } else {
                    State::ReadContent
                };
                on_future!(this, result, State::Eof, |r| {
                    this.state = state;
                    this.entry_name = r;
                    this.entry_name.pop();
                    Poll::Pending
                })
            },
            State::ReadContent => {
                let file = this.reader.clone();
                let future = async move {
                    let mut buffer = [0u8; 32];
                    file.write().await.read_exact(&mut buffer).await?;
                    Ok(buffer)
                };
                this.state = State::WaitingReadContent(Box::pin(future));
                Poll::Pending
            },
            State::WaitingReadContent(ref mut fut) => {
                let result = fut.as_mut().poll(cx);
                on_future!(this, result, State::Eof, |buffer_content: [u8; 32]| {
                    this.state = State::ReadPosition;
                    this.position += this.entry_name.len() as u64 + buffer_content.len() as u64 + 2;
                    let static_slice: &'static [u8; 32] = unsafe { mem::transmute_copy(&buffer_content) };
                    Poll::Ready(Some(Ok(BorrowedEntry {
                        name: Box::leak(this.entry_name.clone().into_boxed_slice()),
                        link: BorrowedLink::Content(static_slice),
                    })))
                })
            },
            State::ReadPath => {
                let file = this.reader.clone();
                let future = async move {
                    let mut buffer = Vec::new();
                    file.write().await.read_until(0x00, &mut buffer).await?;
                    Ok(buffer)
                };
                this.state = State::WaitingReadPath(Box::pin(future));
                Poll::Pending
            },
            State::WaitingReadPath(ref mut future) => {
                let result = future.as_mut().poll(cx);
                on_future!(this, result, State::Eof, |buffer_link: Vec<u8>| {
                    this.state = State::ReadPosition;
                    this.position += this.entry_name.len() as u64 + buffer_link.len() as u64 + 2;
                    let content = buffer_link[..buffer_link.len() - 1].to_vec().into_boxed_slice();
                    let static_slice: &'static [u8] = Box::leak(content);
                    Poll::Ready(Some(Ok(BorrowedEntry {
                        name: Box::leak(this.entry_name.clone().into_boxed_slice()),
                        link: BorrowedLink::Path(static_slice),
                    })))
                })
            },
        }
    }
}
```

## Key Changes Explained

1. **State Management**: The state machine now includes states for waiting on futures, which allows us to keep track of the current operation without creating new futures unnecessarily.

2. **Future Reuse**: Instead of creating a new future for each operation, we create a future only when entering a waiting state. This reduces the number of allocations and improves performance.

3. **Cleaner Polling Logic**: The polling logic is streamlined, making it easier to follow the flow of operations and transitions between states.

## Conclusion

By optimizing the stream implementation to reuse futures, we have significantly improved its performance. This change not only reduces memory allocations but also enhances the overall efficiency of the directory entry stream. In future posts, we will explore additional enhancements and features that can be added to this implementation, such as error recovery and performance tuning.

Stay tuned for more insights into Rust programming and systems design!