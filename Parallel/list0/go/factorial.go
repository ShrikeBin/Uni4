package main

import "fmt"

func factorial(x uint64) uint64 {
	if x == 0 {
		return 1
	}
	return x * factorial(x-1)
}

func main() {
	var x uint64

	fmt.Print("Enter n: ")

	_ , err := fmt.Scan(&x)

	if err != nil{
		fmt.Println("Error reading input")
		return
	}

	fmt.Println(x,"! is:", factorial(x))
}
