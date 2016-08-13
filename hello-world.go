// +build helloWorld

package main

import (
	"fmt"
	"math/rand"
	"time"

	"github.com/smddzcy/learning-go"
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

	// Using packages
	b := binaryTree.New()
	startRec := time.Now().UnixNano()
	b.RecursiveAdd(5)
	b.RecursiveAdd(10)
	b.RecursiveAdd(2)
	b.RecursiveAdd(15)
	b.RecursiveAdd(4)
	b.RecursiveAdd(-5)
	endRec := time.Now().UnixNano()

	b2 := binaryTree.New()
	startLoop := time.Now().UnixNano()
	b2.Add(7)
	b2.Add(12)
	b2.Add(4)
	b2.Add(17)
	b2.Add(6)
	b2.Add(-3)
	endLoop := time.Now().UnixNano()

	b.Show() // => 2 5 10
	fmt.Println()
	b2.Show() // => 2 5 10
	fmt.Println()

	fmt.Println("Size b1:", b.Size())
	fmt.Println("Size b2:", b2.Size())

	fmt.Printf("Recursive binary tree add took: %f microseconds per element\n", float64(endRec-startRec)/3000)
	fmt.Printf("Loop binary tree add took: %f microseconds per element\n", float64(endLoop-startLoop)/3000)
	/*
		Results:
			Recursive binary tree add took: 0.243667 microseconds per element
			Loop binary tree add took: 0.153000 microseconds per element
	*/

	fmt.Println(`wow $1`)
}
