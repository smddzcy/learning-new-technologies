// +build defer

package main

import "fmt"

func main() {
	// Defer
	fmt.Println("\n\nDefer:")
	for i := 0; i < 5; i++ {
		fmt.Printf("%d ", i)
		// Deferred things are added to a stack and executed after func return
		defer fmt.Printf("%d ", i)
	}
}
