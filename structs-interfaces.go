// +build structsInterfaces

package main

import (
	"fmt"
	"math"
)

type geometry interface {
	Area() float64
	Perimeter() float64
}

type square struct {
	sideLength float64
}

type triangle struct {
	sides [3]float64
}

func (s square) Area() float64 {
	return s.sideLength * s.sideLength
}

func (s square) Perimeter() float64 {
	return 4 * s.sideLength
}

func (s triangle) Area() float64 {
	u := (s.sides[0] + s.sides[1] + s.sides[2]) / 2
	return math.Sqrt(u * (u - s.sides[0]) * (u - s.sides[1]) * (u - s.sides[2]))
}

func (s triangle) Perimeter() float64 {
	return s.sides[0] + s.sides[1] + s.sides[2]
}

func main() {
	// Structs, Interfaces
	fmt.Println("\n\nStructs and interfaces:")
	sqr := square{sideLength: 5} // initializing by named parameters
	sqr2 := square{10}           // or without names
	fmt.Printf("Area of the first square must be 25.0: %f\n", sqr.Area())
	fmt.Printf("Area of the second square must be 100.0: %f\n", sqr2.Area())

	triangleSides := [3]float64{3, 4, 5} // arrays
	tri := triangle{triangleSides}
	fmt.Printf("Area of the triangle must be 6.0: %f\n", tri.Area())

	dev, avg := areaDeviation(sqr, sqr2, tri)

	fmt.Printf("Std. deviation: %f, Avg: %f\n", dev, avg)
}

// Calculates the standart deviation of a collection of geometries
// Returns a tuple; deviation & average of areas
func areaDeviation(shapes ...geometry) (dev, avg float64) {
	len := float64(len(shapes))

	// Get avg of areas
	for _, shape := range shapes {
		avg += shape.Area()
	}
	avg /= len

	// Get std. deviation
	for _, shape := range shapes {
		dev += math.Pow(avg-shape.Area(), 2)
	}
	dev = math.Sqrt(dev / len)

	return dev, avg
}
