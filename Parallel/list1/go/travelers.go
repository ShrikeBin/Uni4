package main

import (
	"fmt"
	"time"
)

// Const variables
const (

	NrOfTravelers = 27
	MinSteps      = 10
	MaxSteps      = 100

	MinDelay time.Duration = 10 * time.Millisecond
	MaxDelay time.Duration = 50 * time.Millisecond

	BoardWidth  = 15
	BoardHeight = 15
)

// Set timing
var StartTime time.Time = time.Now()

// Types, procedures and functions

	//Postitions on the board
	type Position_Type struct{
		X int
		Y int
	}

	// elementary steps
	func MoveDown(p *PositionType) {
		p.Y = (p.Y + 1) % BoardHeight
	}

	func MoveUp(p *PositionType) {
		p.Y = (p.Y + BoardHeight -1) % BoardHeight
	}

	func MoveRight(p *PositionType) {
		p.X = (p.X + 1) % BoardWidth
	}

	func MoveLeft(p *PositionType) {
		p.Y = (p.Y + BoardWidth - 1) % BoardWidth
	}

	// traces of travelers
	type Trace_Type struct {
		TimeStamp time.Duration
		ID        int
		Position  PositionType
		Symbol    rune // rune == Character in Ada
	}
	// idk what are those yet
	type Trace_Array_Type [MaxSteps]Trace_Type

	type Traces_Sequence_Type struct{
		Last int = -1
		Trace_Array Trace_Array_Type
	}

	func Print_Trace(trace TraceType) {
		fmt.Printf("%v %d %d %d %s\n", trace.TimeStamp, trace.Id, trace.Position.X, trace.Position.Y, trace.Symbol)
	}

	func Print_Traces(traces TracesSequenceType) {
		for i := 0; i <= traces.Last; i++ {
			Print_Trace(traces.TraceArray[i])
		}
	}

	// Printer task: Goroutine that collects and prints traces, dunno if synchronied tho
	func Printer(reportChannel <-chan TracesSequenceType) {
		for traces := range reportChannel {
			Print_Traces(traces)
		}
	}

	// travelers
	type Traveler_Type struct{
		ID       int
		Symbol   rune
		Position Position_Type
	}

	// 118 linijka w dół utworzyć task jako metody ?? coś jak javowy Thread

