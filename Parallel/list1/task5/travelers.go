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
	DeadlockDelay = 100 * time.Millisecond

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

// our board
var board [BoardWidth][BoardHeight]chan CellMessage

// Mutex cell
type CellMessage struct {
	op      string   // "set", "clear", "check"
	replyCh chan bool
}

// Function for each cell
func cell(cellCh chan CellMessage) {
	occupied := false

	for msg := range cellCh {
		switch msg.op {
		case "set":
			occupied = true
		case "clear":
			occupied = false
		case "check":
			msg.replyCh <- occupied
		}
	}
}

// Traveler struct
type Traveler struct {
	ID       int
	Symbol   rune
	Position Position
	MoveType int
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
	deadline := time.Now().Add(DeadlockDelay)

	for time.Now().Before(deadline) {
		occupiedReply := make(chan bool)
		board[newPos.X][newPos.Y] <- CellMessage{op: "check", replyCh: occupiedReply}

		if <-occupiedReply {
			time.Sleep(1 * time.Millisecond)
			continue
		}

		board[t.Position.X][t.Position.Y] <- CellMessage{op: "clear"}

		board[newPos.X][newPos.Y] <- CellMessage{op: "set"}

		t.Position = newPos
		return true
	}

	return false
}


// Traveler 
func (t *Traveler) Travel(reportChannel chan<- []Trace, wg *sync.WaitGroup) {
	defer wg.Done()

	nrOfSteps := MinSteps + t.Rand.Intn(MaxSteps-MinSteps)
	t.Traces = make([]Trace, 0, nrOfSteps+1)

	board[t.Position.X][t.Position.Y]<- CellMessage{op: "set", replyCh: nil}

	// Store initial
	t.Traces = append(t.Traces, Trace{
		TimeStamp: time.Since(StartTime).Microseconds(),
		ID:        t.ID,
		Position:  t.Position,
		Symbol:    t.Symbol,
	})

	// steps
	for i := 0; i < nrOfSteps; i++ {
		time.Sleep(MinDelay + time.Duration(t.Rand.Int63n(int64(MaxDelay-MinDelay))))

		// Random movement
		newPos := t.Position
		switch t.MoveType {
		case 1:
			newPos.MoveUp()
		case 2:
			newPos.MoveDown()
		case 3:
			newPos.MoveLeft()
		case 4:
			newPos.MoveRight()
		}

		// Try to move 
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
	fmt.Printf("-1 %d %d %d\n", NrOfTravelers, BoardWidth, BoardHeight)

	var travelWg sync.WaitGroup
	var printWg sync.WaitGroup

	reportChannel := make(chan []Trace, NrOfTravelers)

	// Start the printer
	printWg.Add(1)
	go Printer(reportChannel, &printWg)

	// Init the board
	for x := 0; x < BoardWidth; x++ {
		for y := 0; y < BoardHeight; y++ {
			board[x][y] = make(chan CellMessage)
			go cell(board[x][y])                
		}
	}

	// Init travelers
	travelers := make([]*Traveler, NrOfTravelers)
	symbol := 'A'

	rand.Seed(time.Now().UnixNano()) // Initialize seed

	var tmp = 0;

	for i := 0; i < NrOfTravelers; i++ {
		if i/2 == 0 {	// even ID
			if rand.Intn(2) == 0 {
				tmp = 1	// UP
			} else {
				tmp = 2	// DOWN
			}
		} else {	// odd ID
			if rand.Intn(2) == 0 {
				tmp = 3	// LEFT
			} else {
				tmp = 4	// RIGHT
			}
		}

		randSource := rand.NewSource(time.Now().UnixNano() + int64(i))

		travelers[i] = &Traveler{
			ID:       i,
			Symbol:   symbol,
			Position: Position{i, i},
			MoveType:     tmp,
			Rand:     rand.New(randSource),
		}
		symbol++
	}

	// Start each traveler
	for _, traveler := range travelers {
		travelWg.Add(1)
		go traveler.Travel(reportChannel, &travelWg)
	}

	// Wait for travelers
	travelWg.Wait()

	// Close the printer
	close(reportChannel)

	// Wait for the printer to finish
	printWg.Wait()
}

// SPRAWDZ STATEFUL GOROUTINES Z GO BY EXAMPLE (NP TABLICE GORUTYN) taki serwerek na inofrmacje, poczytac w seci
