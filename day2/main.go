
package day2

import (
    "fmt"
    "os"
    "io"
    "bufio"
    "strings"
    "strconv"
)

type Position struct {
    horiz, depth int
}

type Move struct {
    horiz, depth int
}

func f1(moves []Move) Position {
    p := Position{}
    for _, d := range moves {
        p.horiz += d.horiz
        p.depth += d.depth
    }

    return p
}

func parseMoves(r io.Reader) (moves []Move, err error) {
    s := bufio.NewScanner(r)

    for s.Scan() {
        m := Move{}
        ws := strings.Split(s.Text(), " ")
        if len(ws) != 2 {
            err = fmt.Errorf("Wrong move %v\n", ws)
            return
        }
        switch ws[0] {
            case "forward":
                m.horiz, err = strconv.Atoi(ws[1])
                if err != nil {
                    err = fmt.Errorf("Wrong move number %v\n", ws)
                    return
                }
            case "up":
                m.depth, err = strconv.Atoi(ws[1])
                if err != nil {
                    err = fmt.Errorf("Wrong move number %v\n", ws)
                    return
                }
                m.depth = 0 - m.depth
            case "down":
                m.depth, err = strconv.Atoi(ws[1])
                if err != nil {
                    err = fmt.Errorf("Wrong move number %v\n", ws)
                    return
                }
            default:
                moves = nil
                err = fmt.Errorf("Wrong move %v\n", ws)
                return
        }

        fmt.Printf("Move: %#v\n", m)
        moves = append(moves, m)
    }

    return
}
func RunF1(input string) {
    h0, err := os.Open(input)
    if err != nil { return }
    defer h0.Close()

    moves, err := parseMoves(h0)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }
    p := f1(moves)

    fmt.Printf("Answer1: %d (%d, %d)\n", p.horiz * p.depth, p.horiz, p.depth)
}
