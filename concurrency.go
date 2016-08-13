// +build concurrency

package main

import "fmt"

func main() {
	// Asynchronous execution
	ch1 := make(chan bool)
	ch2 := make(chan bool)
	go count(3, "routine 1", ch1)
	go count(5, "routine 2", ch2)

	// Wait for them to finish.
	<-ch1
	<-ch2

	// "make applies only to maps, slices and channels"
	// create a channel

	ch := make(chan string)

	go func() { ch <- "sup?" }() // send a value to the channel
	fmt.Println(<-ch)            // "<-" operator receives the value from channel

	// Receivers always block until there is data to receive.
	// If the channel is unbuffered, the sender blocks until the receiver has received the value.
	// If the channel has a buffer, the sender blocks only until the value has been copied to the buffer;
	// if the buffer is full, this means waiting until some receiver has retrieved a value.
	go func() { ch <- "New message" }()
	// By default, channel msg sends & receives are blocking.
	// So synchronization is not necessary here
	fmt.Println(<-ch)

	bufferedCh := make(chan int, 2)

	bufferedCh <- 1
	bufferedCh <- 2

	fmt.Println(<-bufferedCh)
	fmt.Println(<-bufferedCh)

	// TODO: Concurrent and parallel programming exercises (unbuffered channels)
	// TODO: Semaphore exercise (by using buffered channels)
}

// chan<- means this func can only "send" to this channel.
func count(to int, prefix string, done chan<- bool) {
	for i := 0; i < to; i++ {
		fmt.Println(prefix, ":", i)
	}
	done <- true
}
