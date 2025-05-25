package main

import (
	"fmt"
	"math/rand"
	"time"
	"sync/atomic"
)

const (
	NrOfProcesses = 2
	MinSteps      = 50
	MaxSteps      = 100
	MinDelay      = 10 * time.Millisecond
	MaxDelay      = 50 * time.Millisecond
)

var (
	StartTime = time.Now()
)

type ProcessState int
const (
	LocalSection ProcessState = iota
	EntryProtocol
	CriticalSection
	ExitProtocol
)

var (
	ProcessStateImages = []string{
		"LOCAL_SECTION",
		"ENTRY_PROTOCOL",
		"CRITICAL_SECTION",
		"EXIT_PROTOCOL",
	}
)

const (
	BoardWidth = NrOfProcesses
	BoardHeight = ExitProtocol + 1
)

type Position struct {
	X, Y int
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

var (
	printer Printer
)

func (p *Printer) Start() {
	p.traceChannel = make(chan []Trace, NrOfProcesses)
	p.done = make(chan bool)

	go func() {
		for i := 0; i < NrOfProcesses; i++ {
			traces := <-p.traceChannel
			Print_Traces(traces)
		}

		fmt.Printf("-1 %d %d %d ",
			NrOfProcesses,
			BoardWidth,
			BoardHeight,
		)
		for _, img := range ProcessStateImages {
			fmt.Printf("%s;", img)
		}

		p.done <- true
	}()
}

var (
	Flags = make([]int32, NrOfProcesses) // automatically 0
	isTurn = int32(0)
)

type Process struct {
	Id        int
	Symbol    rune
	Position  Position
	Steps     int
	Traces    []Trace
	TimeStamp time.Duration
	localMaxTicket int32
}

func (t *Process) Store_Trace() {
	t.Traces = append(t.Traces, Trace{
		TimeStamp: t.TimeStamp,
		Id:        t.Id,
		Position:  t.Position,
		Symbol:    t.Symbol,
	})
}

func (t* Process) OtherId() int {
	return (t.Id + 1) % 2
}

func (t* Process) ChangeState(NewState ProcessState) {
	t.Position.Y = int(NewState)
	t.TimeStamp = time.Since(StartTime)
	t.Store_Trace()

	time.Sleep(time.Millisecond) // for printing
}

func (t *Process) Init(id int, symbol rune) {
	t.Id = id
	t.Symbol = symbol

	t.Position = Position{
		X: id,
		Y: int(LocalSection),
	}

	t.Steps = MinSteps + rand.Intn(MaxSteps-MinSteps+1)
	t.TimeStamp = time.Since(StartTime)
	t.Store_Trace()
}

func (t *Process) Start() {
	go func() {
		for i := 0; i < t.Steps / 4 - 1; i++ {
			time.Sleep(MinDelay + time.Duration(rand.Int63n(int64(MaxDelay-MinDelay))))
			
			t.ChangeState(EntryProtocol)

			atomic.StoreInt32(&Flags[t.Id], 1)
			atomic.StoreInt32(&isTurn, int32(t.Id))

			for {
				if atomic.LoadInt32(&Flags[t.OtherId()]) == 0 ||
					atomic.LoadInt32(&isTurn) != int32(t.Id) {
					break
				}
			}

			t.ChangeState(CriticalSection)

			time.Sleep(MinDelay + time.Duration(rand.Int63n(int64(MaxDelay-MinDelay))))

			t.ChangeState(ExitProtocol)

			atomic.StoreInt32(&Flags[t.Id], 0)

			t.ChangeState(LocalSection)
		}

		printer.traceChannel <- t.Traces
	}()
}

func main() {
	var Processes [NrOfProcesses]Process

	printer.Start()

	symbol := 'A'
	for i := 0; i < NrOfProcesses; i++ {
		Processes[i].Init(i, symbol)
		symbol++
	}

	for i := 0; i < NrOfProcesses; i++ {
		Processes[i].Start()
	}

	<-printer.done
}
