package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

// Constants
const (
	NrOfTravelers = 15
	MinSteps      = 10
	MaxSteps      = 100

	MinDelay = 10 * time.Millisecond
	MaxDelay = 50 * time.Millisecond

	BoardWidth  = 15
	BoardHeight = 15
)

// Global start time
var StartTime = time.Now()

// Position on the board
type PositionType struct {
	X int
	Y int
}

// Movement functions (torus topology)
func MoveDown(p *PositionType) {
	p.Y = (p.Y + 1) % BoardHeight
}

func MoveUp(p *PositionType) {
	p.Y = (p.Y + BoardHeight - 1) % BoardHeight
}

func MoveRight(p *PositionType) {
	p.X = (p.X + 1) % BoardWidth
}

func MoveLeft(p *PositionType) {
	p.X = (p.X + BoardWidth - 1) % BoardWidth
}

// Trace of traveler movement
type TraceType struct {
	TimeStamp time.Duration
	ID        int
	Position  PositionType
	Symbol    rune
}

type TracesSequenceType struct {
	Last        int
	TraceArray  []TraceType
}

// Function to print a single trace
func PrintTrace(trace TraceType) {
	fmt.Printf("%v %d %d %d %c\n", trace.TimeStamp, trace.ID, trace.Position.X, trace.Position.Y, trace.Symbol)
}

// Function to print all traces
func PrintTraces(traces TracesSequenceType) {
	for _, trace := range traces.TraceArray {
		PrintTrace(trace)
	}
}

// Printer goroutine to collect and print traces
func Printer(reportChannel <-chan TracesSequenceType, wg *sync.WaitGroup) {
	defer wg.Done()
	for traces := range reportChannel {
		PrintTraces(traces)
	}
}

// Traveler structure
type Traveler struct {
	ID       int
	Symbol   rune
	Position PositionType
	Rand     *rand.Rand // Each traveler has a unique random generator
}

// Board shared between travelers
var board [BoardWidth][BoardHeight]int // Each position will store the traveler ID (0 means unoccupied)
var boardMutex sync.Mutex             // Mutex to synchronize access to the board

// Traveler behavior as a goroutine
func (t *Traveler) Travel(reportChannel chan<- TracesSequenceType, wg *sync.WaitGroup) {
	// signal to waitgroup that we are done
	defer wg.Done()

	// Generate a random number of steps
	nrOfSteps := MinSteps + t.Rand.Intn(MaxSteps-MinSteps)
	var traces TracesSequenceType
	traces.TraceArray = make([]TraceType, 0, nrOfSteps+1)

	// Store initial position
	traces.TraceArray = append(traces.TraceArray, TraceType{
		TimeStamp: time.Since(StartTime),
		ID:        t.ID,
		Position:  t.Position,
		Symbol:    t.Symbol,
	})

	// Mark initial position as occupied
	boardMutex.Lock()
	board[t.Position.X][t.Position.Y] = t.ID
	boardMutex.Unlock()

	// Perform steps
	for i := 0; i < nrOfSteps; i++ {
		time.Sleep(MinDelay + time.Duration(t.Rand.Int63n(int64(MaxDelay-MinDelay))))

		// Random movement
		var newPosition PositionType
		switch t.Rand.Intn(4) {
		case 0:
			MoveUp(&newPosition)
		case 1:
			MoveDown(&newPosition)
		case 2:
			MoveLeft(&newPosition)
		case 3:
			MoveRight(&newPosition)
		}

		// Synchronize access to the board to check and move
		boardMutex.Lock()
		if board[newPosition.X][newPosition.Y] == 0 { // If the position is free
			// Mark old position as unoccupied
			board[t.Position.X][t.Position.Y] = 0
			// Move to the new position
			t.Position = newPosition
			board[t.Position.X][t.Position.Y] = t.ID // Mark new position as occupied
		}
		boardMutex.Unlock()

		// Store trace
		traces.TraceArray = append(traces.TraceArray, TraceType{
			TimeStamp: time.Since(StartTime),
			ID:        t.ID,
			Position:  t.Position,
			Symbol:    t.Symbol,
		})
	}

	// Send collected traces to the printer
	reportChannel <- traces
}

func main() {
	// Print initial debug information
	fmt.Printf("-1 %d %d %d\n", NrOfTravelers, BoardWidth, BoardHeight)

	var travelWg sync.WaitGroup // WaitGroup to track when all travelers finish
	var printWg sync.WaitGroup  // WaitGroup to track when the printer finishes

	// Buffered channel to store reports from travelers
	reportChannel := make(chan TracesSequenceType, NrOfTravelers)

	// Start the printer goroutine
	printWg.Add(1) // Ensure we wait for the printer before exiting
	go Printer(reportChannel, &printWg)

	// Initialize a slice to store traveler pointers
	travelers := make([]*Traveler, NrOfTravelers)
	symbol := 'A' // Start with ASCII 'A' for traveler symbols

	// Create travelers with unique IDs, symbols, and positions
	for i := 0; i < NrOfTravelers; i++ {
		randSource := rand.NewSource(time.Now().UnixNano() + int64(i)) // Unique seed for randomness

		// Initialize a new Traveler struct and assign it to the slice
		travelers[i] = &Traveler{
			ID:       i,
			Symbol:   rune(symbol),
			Position: PositionType{rand.Intn(BoardWidth), rand.Intn(BoardHeight)}, // Random init position
			Rand:     rand.New(randSource), // Separate random generator for each 
		}
		symbol++
	}

	// Start each traveler
	for _, traveler := range travelers {
		travelWg.Add(1) // Increment the WaitGroup counter for each traveler
		go traveler.Travel(reportChannel, &travelWg)
	}

	// Wait for all travelers
	travelWg.Wait()

	// Close the printer channel
	close(reportChannel)

	// Wait for the printer to finish
	printWg.Wait()
}
