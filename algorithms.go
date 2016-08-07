// +build algorithms

package main

import (
	"errors"
	"fmt"
)

func main() {
	x := []int{
		48, 96, 86, 68,
		57, 82, 63, 70,
		37, 34, 83, 27,
		19, 97, 9, 17,
	}
	el, _ := smallestElement(x)
	fmt.Println(el)
}

func smallestElement(arr []int) (int, error) {
	length := len(arr)
	if length == 0 {
		return -1, errors.New("Array is empty.")
	}

	smallest := arr[0]
	for i := 0; i < length; i++ {
		if arr[i] < smallest {
			smallest = arr[i]
		}
	}
	return smallest, nil
}
