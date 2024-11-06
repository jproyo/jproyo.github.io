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
// State machine states for iterating through directory entries
enum State {
    ReadPosition,
    ReadFlag,
    ReadEntryName,
    ReadContent,
    Eof,
    WaitingReadPosition(Pin<Box<dyn Future<Output = Result<u64, std::io::Error>>>>),
    WaitingReadFlag(Pin<Box<dyn Future<Output = Result<u8, std::io::Error>>>>),
    WaitingReadEntryName(Pin<Box<dyn Future<Output = Result<Vec<u8>, std::io::Error>>>>),
    WaitingReadContent(Pin<Box<dyn Future<Output = Result<[u8; 32], std::io::Error>>>>),
}

// Iterator over entries in a B3 directory
pub struct DirEntriesIter<'a> {
    reader: Arc<RwLock<BufReader<File>>>,
    position: u64,
    state: State,
    entry_name: Vec<u8>,
    flag: u8,
    _marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> DirEntriesIter<'a> {
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

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = self.as_mut().get_mut();

        match this.state {
            State::Eof => Poll::Ready(None), // End of stream

            // Step 1: ReadPosition
            State::ReadPosition => {
                this.entry_name.clear();
                this.flag = 0;
                let file = this.reader.clone();
                let pos = this.position;

                // Create a future to seek to the next entry position
                let future = async move {
                    file.write().await.seek(tokio::io::SeekFrom::Start(pos)).await
                };
                this.state = State::WaitingReadPosition(Box::pin(future)); // Transition to WaitingReadPosition
                Poll::Pending // Indicate that the operation is pending
            },

            // Step 2: WaitingReadPosition
            State::WaitingReadPosition(ref mut future) => {
                let result = future.as_mut().poll(cx); // Poll the future
                on_future!(this, result, State::Eof, |r| {
                    this.state = State::ReadFlag; // Transition to ReadFlag
                    Poll::Pending
                })
            },

            // Step 3: ReadFlag
            State::ReadFlag => {
                let file = this.reader.clone();

                // Create a future to read the entry type flag
                let future = async move {
                    file.write().await.read_u8().await
                };
                this.state = State::WaitingReadFlag(Box::pin(future)); // Transition to WaitingReadFlag
                Poll::Pending
            },

            // Step 4: WaitingReadFlag
            State::WaitingReadFlag(ref mut future) => {
                let result = future.as_mut().poll(cx); // Poll the future
                on_future!(this, result, State::Eof, |r| {
                    this.state = State::ReadEntryName; // Transition to ReadEntryName
                    this.flag = r; // Store the flag value
                    Poll::Pending
                })
            },

            // Step 5: ReadEntryName
            State::ReadEntryName => {
                let file = this.reader.clone();

                // Create a future to read the entry name until the null terminator
                let future = async move {
                    let mut buffer = Vec::new();
                    file.write().await.read_until(0x00, &mut buffer).await?;
                    Ok(buffer)
                };
                this.state = State::WaitingReadEntryName(Box::pin(future)); // Transition to WaitingReadEntryName
                Poll::Pending
            },

            // Step 6: WaitingReadEntryName
            State::WaitingReadEntryName(ref mut future) => {
                let result = future.as_mut().poll(cx); // Poll the future
                on_future!(this, result, State::Eof, |r| {
                    this.state = State::ReadContent; // Set the next state
                    this.entry_name = r; // Store the entry name
                    this.entry_name.pop(); // Remove the null terminator
                    Poll::Pending
                })
            },

            // Step 7: ReadContent
            State::ReadContent => {
                let file = this.reader.clone();

                // Create a future to read the content hash
                let future = async move {
                    let mut buffer = [0u8; 32];
                    file.write().await.read_exact(&mut buffer).await?;
                    Ok(buffer)
                };
                this.state = State::WaitingReadContent(Box::pin(future)); // Transition to WaitingReadContent
                Poll::Pending
            },

            // Step 8: WaitingReadContent
            State::WaitingReadContent(ref mut fut) => {
                let result = fut.as_mut().poll(cx); // Poll the future
                on_future!(this, result, State::Eof, |buffer_content: [u8; 32]| {
                    this.state = State::ReadPosition; // Transition back to ReadPosition
                    this.position += this.entry_name.len() as u64 + buffer_content.len() as u64 + 2; // Update position
                    let static_slice: &'static [u8; 32] = unsafe { mem::transmute_copy(&buffer_content) };
                    Poll::Ready(Some(Ok(BorrowedEntry {
                        name: Box::leak(this.entry_name.clone().into_boxed_slice()),
                        link: BorrowedLink::Content(static_slice),
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

## Explanation of the `poll_next` Function

### Step-by-Step Transition

1. **ReadPosition**: The stream starts in the `ReadPosition` state, where it clears the entry name and resets the flag. It creates a future to seek to the next entry position and transitions to `WaitingReadPosition`.

2. **WaitingReadPosition**: In this state, the future created in the previous step is polled. If the operation is complete, it transitions to `ReadFlag`.

3. **ReadFlag**: The stream reads the entry type flag by creating a new future. It transitions to `WaitingReadFlag`.

4. **WaitingReadFlag**: The future is polled here. If successful, it transitions to `ReadEntryName`.

5. **ReadEntryName**: The stream reads the entry name until the null terminator, creating a future for this operation. It transitions to `WaitingReadEntryName`.

6. **WaitingReadEntryName**: The future is polled. Depending on the flag, it transitions to `ReadContent`.

7. **ReadContent**: If the entry is not a symlink, the stream reads the content hash, creating a future for this operation. It transitions to `WaitingReadContent`.

8. **WaitingReadContent**: The future is polled. If successful, it transitions back to `ReadPosition`.

### Use of Waker

The `waker` is a mechanism that allows the asynchronous runtime to notify the task when it is ready to make progress. In this implementation, the `waker` is used to wake the task when the future completes. This is crucial for ensuring that the stream can continue processing without blocking the entire thread.

### Conclusion

By optimizing the stream implementation to reuse futures and clearly defining the transitions between states, we have significantly improved its performance. This change not only reduces memory allocations but also enhances the overall efficiency of the directory entry stream. In future posts, we will explore additional enhancements and features that can be added to this implementation, such as error recovery and performance tuning.

Stay tuned for more insights into Rust programming and systems design!

