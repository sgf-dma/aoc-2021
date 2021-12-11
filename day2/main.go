
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
    horiz, depth, aim int
}

type Move struct {
    horiz, depth, aim int
}

func parseMoves(r io.Reader) ([]Move, error) {
    s := bufio.NewScanner(r)

    moves := make([]Move, 0)
    for s.Scan() {
        m := Move{}
        ws := strings.Split(s.Text(), " ")
        if len(ws) != 2 {
            err := fmt.Errorf("Wrong move %v\n", ws)
            return nil, err
        }
        switch ws[0] {
            case "forward":
                x, err := strconv.Atoi(ws[1])
                if err != nil { return nil, err }
                m.horiz = x

            case "up":
                x, err := strconv.Atoi(ws[1])
                if err != nil { return nil, err }
                m.depth = 0 - x
                m.aim = 0 - x

            case "down":
                x, err := strconv.Atoi(ws[1])
                if err != nil { return nil, err }
                m.depth = x
                m.aim = x

            default:
                err := fmt.Errorf("Wrong move %v\n", ws)
                return nil, err
        }

        //fmt.Printf("Move: %#v\n", m)
        moves = append(moves, m)
    }

    return moves, nil
}

func f1(moves []Move) Position {
    p := Position{}
    for _, m := range moves {
        p.horiz += m.horiz
        p.depth += m.depth
    }
    return p
}

func f2(moves []Move) Position {
    p := Position{}
    for _, m := range moves {
        p.horiz += m.horiz
        p.aim += m.aim
        if m.horiz != 0 {
            p.depth += p.aim * m.horiz
        }
        //fmt.Printf("After %#v result is %#v\n", m, p)
    }
    return p
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

func RunF2(input string) {
    h0, err := os.Open(input)
    if err != nil { return }
    defer h0.Close()

    moves, err := parseMoves(h0)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }
    p := f2(moves)

    fmt.Printf("Answer2: %d (%d, %d)\n", p.horiz * p.depth, p.horiz, p.depth)
}
