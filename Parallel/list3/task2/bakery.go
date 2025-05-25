package main

import (
	"fmt"
	"math/rand"
	"time"
	"sync/atomic"
)

const (
	NrOfProcesses = 15
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

type Max_ticket struct {
	SetChannel chan int32
	GetChannel chan (chan int32)
	ticket int32
}

func (m* Max_ticket) Start() {
	m.GetChannel = make(chan (chan int32))
	m.SetChannel = make(chan int32)

	go func() {
		for {
			select {
			case newTicket := <-m.SetChannel:
				if newTicket > m.ticket {
					m.ticket = newTicket
				}
			case responseChan := <-m.GetChannel:
				responseChan <- m.ticket
			}
		}
	}()
}

type Printer struct {
	traceChannel chan []Trace
	done         chan bool
}

var (
	printer Printer
	maxTicket Max_ticket
)

func (p *Printer) Start() {
	p.traceChannel = make(chan []Trace, NrOfProcesses)
	p.done = make(chan bool)

	go func() {
		for i := 0; i < NrOfProcesses; i++ {
			traces := <-p.traceChannel
			Print_Traces(traces)
		}

		response := make(chan int32)
		maxTicket.GetChannel <- response
		ticket := <-response

		fmt.Printf("-1 %d %d %d ",
			NrOfProcesses,
			BoardWidth,
			BoardHeight,
		)
		for _, img := range ProcessStateImages {
			fmt.Printf("%s;", img)
		}
		fmt.Printf("MAX_TICKET=%d;", ticket)

		p.done <- true
	}()
}

var (
	isChoosing = make([]int32, NrOfProcesses)
	Tickets = make([]int32, NrOfProcesses)
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

func (t* Process) Max() int32 {
	current := int32(0)
	for n := range Tickets {
		if atomic.LoadInt32(&Tickets[n]) > current {
			current = atomic.LoadInt32(&Tickets[n])
		}
	}
	return current
}

func (t* Process) ChangeState(NewState ProcessState) {
	t.Position.Y = int(NewState)
	t.TimeStamp = time.Since(StartTime)
	t.Store_Trace()

	time.Sleep(time.Millisecond)
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

			atomic.StoreInt32(&isChoosing[t.Id], 1)
			atomic.StoreInt32(&Tickets[t.Id], 1 + t.Max())
			atomic.StoreInt32(&isChoosing[t.Id], 0)

			if atomic.LoadInt32(&Tickets[t.Id]) > t.localMaxTicket {
				t.localMaxTicket = atomic.LoadInt32(&Tickets[t.Id])
			}

			for j := 0; j < NrOfProcesses; j++ {
				if j != t.Id {
					for {
						if atomic.LoadInt32(&isChoosing[j]) == 0 {
							break
						}
					}
					for {
						if atomic.LoadInt32(&Tickets[j]) == 0 ||
							atomic.LoadInt32(&Tickets[t.Id]) < atomic.LoadInt32(&Tickets[j]) ||
							(atomic.LoadInt32(&Tickets[t.Id]) == atomic.LoadInt32(&Tickets[j]) && t.Id < j) {
							break
						}
					}
				}
			}

			t.ChangeState(CriticalSection)

			time.Sleep(MinDelay + time.Duration(rand.Int63n(int64(MaxDelay-MinDelay))))

			t.ChangeState(ExitProtocol)

			atomic.StoreInt32(&Tickets[t.Id], 0)

			t.ChangeState(LocalSection)
		}

		maxTicket.SetChannel <- t.localMaxTicket
		printer.traceChannel <- t.Traces
	}()
}

func main() {
	var Processes [NrOfProcesses]Process

	printer.Start()
	maxTicket.Start()

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
