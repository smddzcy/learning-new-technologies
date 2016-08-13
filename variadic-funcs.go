package main

import "fmt"

func main() {
	fmt.Println(sum(1, 2, 3, 4))
	fmt.Println(sum(5, 6))
	// or use slices
	arr := []int{1, 2, 3, 4}
	fmt.Println(sum(arr...))
}

func sum(list ...int) (sum int) {
	sum = 0
	for i := 0; i < len(list); i++ {
		sum += list[i]
	}
	return
}
