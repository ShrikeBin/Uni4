package main

import (
	"fmt"
	"math/rand"
	"time"
	"unicode"
)

const (
	NrOfTravelers     = 15
	NrOfWildTravelers = 10
	MinSteps          = 10
	MaxSteps          = 100
	MinDelay          = 10 * time.Millisecond
	MaxDelay          = 50 * time.Millisecond
	BoardWidth        = 15
	BoardHeight       = 15
)

var StartTime = time.Now()

type Position struct {
	X, Y int
}

func Move_Down(pos *Position) {
	pos.Y = (pos.Y + 1) % BoardHeight
}

func Move_Up(pos *Position) {
	pos.Y = (pos.Y + BoardHeight - 1) % BoardHeight
}

func Move_Right(pos *Position) {
	pos.X = (pos.X + 1) % BoardWidth
}

func Move_Left(pos *Position) {
	pos.X = (pos.X + BoardWidth - 1) % BoardWidth
}

type Trace struct {
	TimeStamp time.Duration
	Id        int
	Position  Position
	Symbol    rune
}

func Print_Traces(traces []Trace) {
	for _, trace := range traces {
		fmt.Printf("%f %d %d %d %c\n",
			float64(trace.TimeStamp)/float64(time.Second),
			trace.Id,
			trace.Position.X,
			trace.Position.Y,
			trace.Symbol,
		)
	}
}

type Printer struct {
	traceChannel chan []Trace
	done         chan bool
}

func (p *Printer) Start() {
	p.traceChannel = make(chan []Trace, NrOfTravelers)
	p.done = make(chan bool)

	go func() {
		for i := 0; i < NrOfTravelers; i++ {
			traces := <-p.traceChannel
			Print_Traces(traces)
		}
		p.done <- true
	}()
}

// Each request includes value and an exit channel
type Request struct {
	op     		string
	result chan string
}

type Node struct {
	occupied bool
	reqChan chan Request
}

func (n *Node) Start() {
	go func() {
		n.occupied = false
		for req := range n.reqChan {
			switch req.op {
			case "enter":
				if !n.occupied {
					n.occupied = true
					req.result <- "success"
				} else {
					req.result <- "deny"
				}
			case "leave":
				n.occupied = false
				req.result <- "success"
			}
		}
	}()
}

type Traveler struct {
	Id        int
	Symbol    rune
	Position  Position
	Steps     int
	Traces    []Trace
	TimeStamp time.Duration
	Direction int
}

func (t *Traveler) Store_Trace() {
	// Store the current state of the traveler
	t.Traces = append(t.Traces, Trace{
		TimeStamp: t.TimeStamp,
		Id:        t.Id,
		Position:  t.Position,
		Symbol:    t.Symbol,
	})
}

func (t *Traveler) Init(id int, symbol rune, board *Board) {
	t.Id = id
	t.Symbol = symbol
	t.Position = Position{X: id, Y: 5} // TEMPORARY
	req := Request{op: "enter", result: make(chan string)}

	// Retry indefinitely until "enter" succeeds
	for {
		board[t.Position.X][t.Position.Y].reqChan <- req
		resp := <-req.result
		if resp == "success" {
			break 
		}
	}

	t.Store_Trace()
	t.Steps = MinSteps + rand.Intn(MaxSteps-MinSteps+1)
	t.Direction = (2 * (id % 2)) + rand.Intn(2)
	t.TimeStamp = time.Since(StartTime)
}



func (t *Traveler) Start(printer *Printer, board *Board) {
	go func() {
		newPosition := t.Position

		// Loop for the number of steps 
		for i := 0; i < t.Steps; i++ {
			time.Sleep(MinDelay + time.Duration(rand.Int63n(int64(MaxDelay-MinDelay))))
			Make_Step(&newPosition, t.Direction)
			req := Request{op: "enter", result: make(chan string)}

			successchannel := make(chan bool)
			deadlockchannel := make(chan bool)
			go func() {
				defer close(successchannel) // clean exit
				for{
					board[newPosition.X][newPosition.Y].reqChan <- req
					select {
						case resp := <-req.result:
							if resp == "success" {
								successchannel <- true
								return;
							} else {
								time.Sleep(5*time.Millisecond)
							}
						case <-deadlockchannel:
							return;
					}
				}
			}()


			select {
				case <-successchannel:
					leaveReq := Request{op: "leave", result: make(chan string)}
					board[t.Position.X][t.Position.Y].reqChan <- leaveReq
					<-leaveReq.result

					t.Position = newPosition
					t.Store_Trace()
					t.TimeStamp = time.Since(StartTime)
				case <-time.After(6 * MaxDelay):
					// Deadlock detected, sorry
					deadlockchannel <- true
					t.Symbol = unicode.ToLower(t.Symbol)
					t.Store_Trace()
			}
			
		}

		leaveReq := Request{op: "leave", result: make(chan string)}
		board[t.Position.X][t.Position.Y].reqChan <- leaveReq
		<-leaveReq.result

		printer.traceChannel <- t.Traces
	}()
}


func Make_Step(position *Position, direction int) {
	switch direction {
	case 0:
		Move_Up(position)
	case 1:
		Move_Down(position)
	case 2:
		Move_Left(position)
	case 3:
		Move_Right(position)
	}
}

type Board [BoardWidth][BoardHeight]Node

func (b *Board) Start() {
	for x := 0; x < BoardWidth; x++ {
		for y := 0; y < BoardHeight; y++ {
			b[x][y] = Node{reqChan: make(chan Request)}
			b[x][y].Start()
		}
	}
}

func main() {
	var board Board
	var travelers [NrOfTravelers]Traveler
	var printer Printer

	fmt.Printf("-1 %d %d %d\n", NrOfTravelers, BoardWidth, BoardHeight)

	board.Start()
	printer.Start()

	symbol := 'A'
	for i := 0; i < NrOfTravelers; i++ {
		travelers[i].Init(i, symbol, &board)
		travelers[i].Start(&printer, &board)
		symbol++
	}

	<-printer.done
}
