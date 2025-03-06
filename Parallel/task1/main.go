package main

import "fmt"

// Recursive function to calculate factorial
func factorial(x int) int {
	if x == 0 {
		return 1
	}
	return x * factorial(x-1)
}

func main() {
	var x = 20
	fmt.Println(x,"! is:", factorial(x))
}
