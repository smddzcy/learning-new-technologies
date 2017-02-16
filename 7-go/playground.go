package main

import (
	"fmt"

	"github.com/smddzcy/learning-go/arrays"
)

func main() {
	grades := []int{
		30,
		56,
		46, 20, 18,
		23, 27, 19, 24, 57,
		36,
		32,
		21, 21, 18, 21,
		4, 26, 21, 57, 11, 36, 21, 29, 1,
		44, 15, 46, 39, 33, 36, 24, 16, 18, 14, 16, 4, 12, 5, 28,
		9, 39, 1,
		26, 32, 14, 19, 20, 53,
		26, 16, 24, 11, 62,
		31, 23, 26, 21,
		43, 19, 26,
		28, 29, 31, 29, 32, 30, 31, 47, 48, 6,
	}

	fmt.Println(float64(arrays.Avg(grades)*2/3+22+14) * 1.2)
	fmt.Println(arrays.Mod([]int{1, 2, 3, 1, 1, 1, 1, 2}))
}
