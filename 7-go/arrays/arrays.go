package arrays

import (
	"math"
	"sort"
)

// Sum gives the sum of an array
func Sum(arr []int) int {
	sum := 0
	for _, el := range arr {
		sum += el
	}
	return sum
}

// Avg gives the average of an array
func Avg(arr []int) int {
	return Sum(arr) / len(arr)
}

// Mean gives the average of an array
// Just a link for Avg method
func Mean(arr []int) int {
	return Avg(arr)
}

// Med gives the middle element of an array
func Med(arr []int) int {
	newArr := make([]int, len(arr))
	copy(newArr, arr)
	sort.Ints(newArr)
	return newArr[len(newArr)/2]
}

// Mod gives the most recurring element of an array
func Mod(arr []int) int {
	recurrences := map[int]int{}

	// set recurrences
	for _, el := range arr {
		recurrences[el]++
	}

	// get the most recurred element
	maxOccurrence := math.MinInt32
	el := 0

	for e, recurrence := range recurrences {
		if recurrence > maxOccurrence {
			maxOccurrence = recurrence
			el = e
		}
	}

	return el
}
