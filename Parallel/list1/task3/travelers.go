package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
	"unicode"
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

var StartTime = time.Now()

type Position struct {
	X int
	Y int
}

func (p *Position) MoveDown() {
	p.Y = (p.Y + 1) % BoardHeight
}

func (p *Position) MoveUp() {
	p.Y = (p.Y + BoardHeight - 1) % BoardHeight
}

func (p *Position) MoveRight() {
	p.X = (p.X + 1) % BoardWidth
}

func (p *Position) MoveLeft() {
	p.X = (p.X + BoardWidth - 1) % BoardWidth
}

// Trace of traveler 
type Trace struct {
	TimeStamp int64
	ID        int
	Position  Position
	Symbol    rune
}

// PositionMutex -> single board position 
type PositionMutex struct {
	sync.Mutex
	occupied   bool
	occupantID int
}

// Board of position mutexes
var board [BoardWidth][BoardHeight]PositionMutex

// Traveler struct
type Traveler struct {
	ID       int
	Symbol   rune
	Position Position
	Rand     *rand.Rand
	Traces   []Trace
}

// Printer function to print traces
func Printer(reportChannel <-chan []Trace, wg *sync.WaitGroup) {
	defer wg.Done()
	for traces := range reportChannel {
		for _, trace := range traces {
			fmt.Printf("%v %d %d %d %c\n", 
				trace.TimeStamp, 
				trace.ID, 
				trace.Position.X, 
				trace.Position.Y, 
				trace.Symbol)
		}
	}
}


func (t *Traveler) tryMove(newPos Position, maxDelay time.Duration) bool {
    deadline := time.Now().Add(maxDelay)
    
    for time.Now().Before(deadline) {
        
        if board[newPos.X][newPos.Y].TryLock() {
            
            if board[newPos.X][newPos.Y].occupied {
                board[newPos.X][newPos.Y].Unlock()
                // wait for it to maybe free
                time.Sleep(1 * time.Millisecond)
                continue
            }

            acquiredCurrentNode := false
            select {
            case <-time.After(time.Until(deadline)):
                board[newPos.X][newPos.Y].Unlock()
                return false
            default:
                if board[t.Position.X][t.Position.Y].TryLock() {
                    acquiredCurrentNode = true
                }
            }

            if acquiredCurrentNode {
                // Perform the move
                board[t.Position.X][t.Position.Y].occupied = false
                board[t.Position.X][t.Position.Y].occupantID = -1
                board[t.Position.X][t.Position.Y].Unlock()

                t.Position = newPos
                board[newPos.X][newPos.Y].occupied = true
                board[newPos.X][newPos.Y].occupantID = t.ID
                board[newPos.X][newPos.Y].Unlock()
                return true
            }

            // Failed to acquire current position
            board[newPos.X][newPos.Y].Unlock()
        }

        // Small sleep to prevent CPU spin
        time.Sleep(1 * time.Millisecond)
    }
    
    return false
}

// Traveler behavior as a goroutine
func (t *Traveler) Travel(reportChannel chan<- []Trace, wg *sync.WaitGroup) {
	defer wg.Done()

	// Generate a random number of steps
	nrOfSteps := MinSteps + t.Rand.Intn(MaxSteps-MinSteps)
	t.Traces = make([]Trace, 0, nrOfSteps+1)

	// Initialize and occupy starting position
	board[t.Position.X][t.Position.Y].Lock()
	board[t.Position.X][t.Position.Y].occupied = true
	board[t.Position.X][t.Position.Y].occupantID = t.ID
	board[t.Position.X][t.Position.Y].Unlock()

	// Store initial position
	t.Traces = append(t.Traces, Trace{
		TimeStamp: time.Since(StartTime).Microseconds(),
		ID:        t.ID,
		Position:  t.Position,
		Symbol:    t.Symbol,
	})

	// Perform steps
	for i := 0; i < nrOfSteps; i++ {
		time.Sleep(MinDelay + time.Duration(t.Rand.Int63n(int64(MaxDelay-MinDelay))))

		// Random movement
		newPos := t.Position
		switch t.Rand.Intn(4) {
		case 0:
			newPos.MoveUp()
		case 1:
			newPos.MoveDown()
		case 2:
			newPos.MoveLeft()
		case 3:
			newPos.MoveRight()
		}

		// Try to move with timeout
		if !t.tryMove(newPos, MaxDelay) {
			// If couldn't move, change to lowercase and terminate
			t.Symbol = unicode.ToLower(t.Symbol)
			t.Traces = append(t.Traces, Trace{
				TimeStamp: time.Since(StartTime).Microseconds(),
				ID:        t.ID,
				Position:  t.Position,
				Symbol:    t.Symbol,
			})
			break
		}

		// Store trace
		t.Traces = append(t.Traces, Trace{
			TimeStamp: time.Since(StartTime).Microseconds(),
			ID:        t.ID,
			Position:  t.Position,
			Symbol:    t.Symbol,
		})
	}

	// Send collected traces to the printer
	reportChannel <- t.Traces
}

func main() {
	// Print initial debug information
	fmt.Printf("-1 %d %d %d\n", NrOfTravelers, BoardWidth, BoardHeight)

	var travelWg sync.WaitGroup
	var printWg sync.WaitGroup

	// Buffered channel to store reports from travelers
	reportChannel := make(chan []Trace, NrOfTravelers)

	// Start the printer goroutine
	printWg.Add(1)
	go Printer(reportChannel, &printWg)

	// Initialize travelers
	travelers := make([]*Traveler, NrOfTravelers)
	symbol := 'A'

	for i := 0; i < NrOfTravelers; i++ {
		randSource := rand.NewSource(time.Now().UnixNano() + int64(i))

		travelers[i] = &Traveler{
			ID:       i,
			Symbol:   symbol,
			Position: Position{rand.Intn(BoardWidth), rand.Intn(BoardHeight)},
			Rand:     rand.New(randSource),
		}
		symbol++
	}

	// Start each traveler
	for _, traveler := range travelers {
		travelWg.Add(1)
		go traveler.Travel(reportChannel, &travelWg)
	}

	// Wait for all travelers
	travelWg.Wait()

	// Close the printer channel
	close(reportChannel)

	// Wait for the printer to finish
	printWg.Wait()
}