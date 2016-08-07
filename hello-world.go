// +build helloWorld

package main

import (
	"fmt"
	"math/rand"
	"time"
)

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

	// Conditional statements
	fmt.Println("\n\nConditionals:")
	if true && false {
		fmt.Println("Never executes.")
	} else {
		fmt.Println("👍")
	}

	rand.Seed(time.Now().UnixNano()) // seed the random number generator
	num := rand.Intn(3) + 1          // random number between 1 and 3
	fmt.Println("Random num: ", num)
	switch num {
	case 1:
		fmt.Println("Low")
		break
	case 2:
		fmt.Println("Middle")
		break
	case 3:
		fmt.Println("High")
		break
	}
}
