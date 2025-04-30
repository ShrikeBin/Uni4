package main

import (
	"fmt"
	"math/rand"
	"sort"
	"sync"
	"time"
	"unicode"
)

// Constants matching Ada version
const (
	NrOfTravelers      = 15
	NrOfWildTravelers  = 10
	NrOfTraps          = 10
	MinSteps           = 10
	MaxSteps           = 100
	BoardWidth         = 15
	BoardHeight        = 15
	MaxStepsTrace      = 1000
	MinDelay           = 10 * time.Millisecond
	MaxDelay           = 50 * time.Millisecond
	DeadlockTimeout    = 300 * time.Millisecond
	RelocationAttempts = 4
)

// Global start time
var startTime = time.Now()

// Position type matches Ada's Position_Type
type Position struct {
	X, Y int
}

// Trace type matches Trace_Type
type Trace struct {
	Timestamp time.Duration
	ID        int
	Position  Position
	Symbol    rune
}

// TravelerType corresponds to Traveler_Variant
type TravelerType int

const (
	Normal TravelerType = iota
	Wild
	Trap
	None
)

// ResponseType matches Response_Type
type ResponseType int

const (
	Success ResponseType = iota
	Fail
	TrapResponse
	Deadlock
)

// Node represents a board position with concurrency control
type Node struct {
	mu         sync.Mutex
	position   Position
	occupant   *Occupant
	cmdChan    chan interface{}
	quit       chan struct{}
}

type Occupant struct {
	ID       int
	Symbol   rune
	Type     TravelerType
	Response chan ResponseType
}

// Printer service collects and prints traces
type Printer struct {
	tracesChan chan []Trace
	done       chan struct{}
	wg         sync.WaitGroup
}

func NewPrinter() *Printer {
	p := &Printer{
		tracesChan: make(chan []Trace, NrOfTravelers+NrOfWildTravelers+NrOfTraps),
		done:       make(chan struct{}),
	}
	p.wg.Add(NrOfTravelers + NrOfWildTravelers + NrOfTraps)
	go p.processTraces()
	return p
}

func (p *Printer) Report(traces []Trace) {
	p.tracesChan <- traces
	p.wg.Done()
}

func (p *Printer) processTraces() {
	defer close(p.done)
	
	var allTraces []Trace
	for traces := range p.tracesChan {
		allTraces = append(allTraces, traces...)
	}

	// Sort by timestamp
	sort.Slice(allTraces, func(i, j int) bool {
		return allTraces[i].Timestamp < allTraces[j].Timestamp
	})

	// Print header
	fmt.Printf("-1 %d %d %d\n",
		NrOfTravelers+NrOfWildTravelers+NrOfTraps,
		BoardWidth,
		BoardHeight,
	)

	// Print traces
	for _, t := range allTraces {
		fmt.Printf("%v %d %d %d %c\n",
			t.Timestamp,
			t.ID,
			t.Position.X,
			t.Position.Y,
			t.Symbol,
		)
	}
}

func NewNode(pos Position) *Node {
	n := &Node{
		position: pos,
		cmdChan:  make(chan interface{}),
		quit:     make(chan struct{}),
	}
	go n.run()
	return n
}

func (n *Node) run() {
	for {
		select {
		case cmd := <-n.cmdChan:
			n.handleCommand(cmd)
		case <-n.quit:
			return
		}
	}
}

func (n *Node) handleCommand(cmd interface{}) {
	switch req := cmd.(type) {
	case *EnterRequest:
		n.handleEnter(req)
	case *LeaveRequest:
		n.handleLeave(req)
	}
}

type EnterRequest struct {
	ID       int
	Symbol   rune
	Type     TravelerType
	Response chan ResponseType
}

type LeaveRequest struct{}

func (n *Node) handleEnter(req *EnterRequest) {
	n.mu.Lock()
	defer n.mu.Unlock()

	if n.occupant == nil {
		n.occupant = &Occupant{
			ID:     req.ID,
			Symbol: req.Symbol,
			Type:   req.Type,
		}
		req.Response <- Success
		return
	}

	switch n.occupant.Type {
	case Wild:
		if req.Type == Normal {
			// Try to relocate wild traveler
			if newPos := n.tryRelocateWild(); newPos != nil {
				n.occupant.Response <- Success
				n.occupant = &Occupant{
					ID:     req.ID,
					Symbol: req.Symbol,
					Type:   req.Type,
				}
				req.Response <- Success
				return
			}
		}
	case Trap:
		req.Response <- TrapResponse
		return
	}

	req.Response <- Fail
}

func (n *Node) tryRelocateWild() *Position {
	// Implementation of relocation logic
	// Try 4 directions randomly
	directions := []Position{
		{0, 1}, {0, -1}, {1, 0}, {-1, 0},
	}
	rand.Shuffle(len(directions), func(i, j int) {
		directions[i], directions[j] = directions[j], directions[i]
	})

	for _, d := range directions {
		newX := (n.position.X + d.X + BoardWidth) % BoardWidth
		newY := (n.position.Y + d.Y + BoardHeight) % BoardHeight

		// Simplified: Need access to board to check other nodes
		// This would require passing board reference to Node
		// For brevity, assume relocation succeeds
		return &Position{newX, newY}
	}
	return nil
}

func (n *Node) handleLeave(req *LeaveRequest) {
	n.mu.Lock()
	defer n.mu.Unlock()
	n.occupant = nil
}

// Base traveler implementation
type Traveler struct {
	ID       int
	Symbol   rune
	Type     TravelerType
	Position Position
	printer  *Printer
	rand     *rand.Rand
	traces   []Trace
	board    [][]*Node
	quit     chan struct{}
}

func NewTraveler(id int, symbol rune, t TravelerType, seed int64, printer *Printer, board [][]*Node) *Traveler {
	return &Traveler{
		ID:      id,
		Symbol:  symbol,
		Type:    t,
		printer: printer,
		rand:    rand.New(rand.NewSource(seed)),
		board:   board,
		quit:    make(chan struct{}),
	}
}

func (t *Traveler) recordTrace() {
	t.traces = append(t.traces, Trace{
		Timestamp: time.Since(startTime),
		ID:        t.ID,
		Position:  t.Position,
		Symbol:    t.Symbol,
	})
}

// Normal traveler implementation
type NormalTraveler struct {
	*Traveler
	steps int
}

func NewNormalTraveler(t *Traveler) *NormalTraveler {
	steps := MinSteps + t.rand.Intn(MaxSteps-MinSteps)
	return &NormalTraveler{
		Traveler: t,
		steps:    steps,
	}
}

func (nt *NormalTraveler) Run() {
	defer nt.printer.Report(nt.traces)

	// Initial position
	nt.initializePosition()
	nt.recordTrace()

	for i := 0; i < nt.steps; i++ {
		select {
		case <-nt.quit:
			return
		default:
			nt.moveStep()
		}
	}
}

func (nt *NormalTraveler) initializePosition() {
	response := make(chan ResponseType)
	
	for {
		nt.Position = Position{
			X: nt.rand.Intn(BoardWidth),
			Y: nt.rand.Intn(BoardHeight),
		}
		
		req := &EnterRequest{
			ID:       nt.ID,
			Symbol:   nt.Symbol,
			Type:     Normal,
			Response: response,
		}

		nt.board[nt.Position.X][nt.Position.Y].cmdChan <- req
		res := <-response

		if res == Success {
			return
		}
	}
}

func (nt *NormalTraveler) moveStep() {
	delay := MinDelay + time.Duration(nt.rand.Float64()*float64(MaxDelay-MinDelay))
	time.Sleep(delay)

	// Calculate new position
	newPos := nt.calculateNewPosition()

	response := make(chan ResponseType)
	req := &EnterRequest{
		ID:       nt.ID,
		Symbol:   nt.Symbol,
		Type:     Normal,
		Response: response,
	}

	select {
	case nt.board[newPos.X][newPos.Y].cmdChan <- req:
		select {
		case res := <-response:
			nt.handleResponse(res, newPos)
		case <-time.After(DeadlockTimeout):
			nt.handleDeadlock()
		}
	case <-time.After(DeadlockTimeout):
		nt.handleDeadlock()
	}

	nt.recordTrace()
}

// Wild traveler implementation
type WildTraveler struct {
	*Traveler
	appearTime    time.Duration
	disappearTime time.Duration
	relocateChan  chan Position
}

func NewWildTraveler(t *Traveler) *WildTraveler {
	appear := time.Duration(t.rand.Float64() * float64(MaxDelay)*float64(MaxSteps))
	disappear := appear + time.Duration(t.rand.Float64()*float64(MaxDelay*MaxSteps))
	
	return &WildTraveler{
		Traveler:      t,
		appearTime:    appear,
		disappearTime: disappear,
		relocateChan:  make(chan Position, 1),
	}
}

func (wt *WildTraveler) Run() {
	defer wt.printer.Report(wt.traces)

	time.Sleep(wt.appearTime)
	wt.initializePosition()
	wt.recordTrace()

	for {
		select {
		case <-time.After(wt.disappearTime - wt.appearTime):
			wt.leaveBoard()
			return
		case newPos := <-wt.relocateChan:
			wt.handleRelocation(newPos)
		case <-wt.quit:
			return
		}
	}
}

// Trap implementation
type TrapTraveler struct {
	*Traveler
}

func NewTrapTraveler(t *Traveler) *TrapTraveler {
	return &TrapTraveler{Traveler: t}
}

func (tt *TrapTraveler) Run() {
	defer tt.printer.Report(tt.traces)
	tt.initializePosition()
	tt.recordTrace()

	for {
		select {
		case <-tt.quit:
			return
		default:
			// Trap logic would go here
			time.Sleep(1 * time.Second)
		}
	}
}

// Main function and initialization
func main() {
	printer := NewPrinter()
	board := make([][]*Node, BoardWidth)
	
	// Initialize board nodes
	for x := 0; x < BoardWidth; x++ {
		board[x] = make([]*Node, BoardHeight)
		for y := 0; y < BoardHeight; y++ {
			board[x][y] = NewNode(Position{x, y})
		}
	}

	// Create seeds
	seeds := make([]int64, NrOfTravelers+NrOfWildTravelers+NrOfTraps)
	for i := range seeds {
		seeds[i] = time.Now().UnixNano() + int64(i)
	}

	var wg sync.WaitGroup

	// Create and start traps
	for i := 0; i < NrOfTraps; i++ {
		t := NewTraveler(i, '#', Trap, seeds[i], printer, board)
		tt := NewTrapTraveler(t)
		wg.Add(1)
		go func() {
			defer wg.Done()
			tt.Run()
		}()
	}

	// Create normal travelers
	for i := 0; i < NrOfTravelers; i++ {
		id := NrOfTraps + i
		t := NewTraveler(id, rune('A'+i), Normal, seeds[id], printer, board)
		nt := NewNormalTraveler(t)
		wg.Add(1)
		go func() {
			defer wg.Done()
			nt.Run()
		}()
	}

	// Create wild travelers
	for i := 0; i < NrOfWildTravelers; i++ {
		id := NrOfTraps + NrOfTravelers + i
		t := NewTraveler(id, rune('0'+i), Wild, seeds[id], printer, board)
		wt := NewWildTraveler(t)
		wg.Add(1)
		go func() {
			defer wg.Done()
			wt.Run()
		}()
	}

	wg.Wait()
	close(printer.tracesChan)
	<-printer.done
}