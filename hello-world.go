// +build helloWorld

package main

import "fmt"

// Basics
func main() {
	fmt.Println("Hello, World!")
	// var s string = "Wow!"// variable
	// s := "Wow!" // variable, Go infers the type
	const s = "Wow 🤔 !" // constant

	// For loop
	fmt.Println("\nFor loop:")
	for pos, char := range s {
		fmt.Printf("Character: ' %c ', Byte pos: %d\n", char, pos)
	}

	// While loop
	fmt.Println("\nWhile loop:")
	j := 1
	for j < 5 {
		fmt.Print(j)
		j++
	}

	// TODO: Other basics

}
