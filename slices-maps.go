// +build defer

package main

import (
	"fmt"
	"strings"
)

func main() {
	// Slices and maps
	fmt.Println("\n\nSlices:")

	slice := make([]int, 0)
	for i := 0; i < 5; i++ {
		// slice[i] = i // panic: runtime error: index out of range
		slice = append(slice, i)
	}
	fmt.Println(slice) // [0 1 2 3 4], appends to end.

	slice2 := make([]int, 2)         // [0,0]
	slice = append(slice, slice2...) // append a slice to another
	fmt.Println(slice)               // [0 1 2 3 4 0 0]

	fmt.Println(slice[1:3]) // [1 2]

	fmt.Println("\n\nMaps:")

	strMap := map[string]func(string) string{
		"append1": func(el string) string {
			return el + "1"
		},
		"upperCase": func(el string) string {
			return strings.ToUpper(el)
		}, // last comma is necessary
	}
	fmt.Printf("el1: %s", strMap["upperCase"]("wow"))
}
