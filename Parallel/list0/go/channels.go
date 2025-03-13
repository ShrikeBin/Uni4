package main

import "fmt"

func main() {

    messages := make(chan string)

    go func() { messages <- "ping" }()

	// passed to other goroutine (main goroutine (?)) and printed
    msg := <-messages
    fmt.Println(msg)
}