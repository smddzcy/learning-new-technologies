package main

import "fmt"

func main() {
	nextInt := intSequence(0)
	fmt.Println(nextInt()) // => 1
	fmt.Println(nextInt()) // => 2
	fmt.Println(nextInt()) // => 3
}

// closes over start, makes a closure
func intSequence(start int) func() int {
	return func() int {
		start += 1
		return start
	}
}
