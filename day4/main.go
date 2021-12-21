
package day4

import (
    "fmt"
    "os"
    "strings"
    "strconv"
    "bufio"
    "io"
)

func readDrawn(s *bufio.Scanner) ([]int, error) {
    line := s.Text()
    nums := make([]int, 0, 32)
    for _, c := range strings.Split(line, ",") {
        x, err := strconv.Atoi(c)
        if err != nil { return nil, err }
        nums = append(nums, x)
    }

    for s.Scan() {
        if len(s.Text()) != 0 { break }
    }

    return nums, nil
}

func readBoardLine(line string) (nums [5]int, err error) {
    s := bufio.NewScanner(strings.NewReader(line))
    s.Split(bufio.ScanWords)

    i := 0
    notEOF := s.Scan()
    for ; i < 5 && notEOF; i++ {
        c := s.Text()
        //fmt.Printf("Read %v\n", c)
        nums[i], err = strconv.Atoi(c)
        if err != nil { return }
        notEOF = s.Scan()
        //fmt.Printf("notEOF = %v\n", notEOF)
    }
    if i < 5 {
        err = fmt.Errorf("Too short board row '%v'\n", line)
    } else if notEOF {
        err = fmt.Errorf("Too long board row '%v'\n", line)
    }
    return
}

type Board struct {
    board [5][5]int
    markedCol  [5]int
    markedLine [5]int
}

func readBoard(s *bufio.Scanner) (b Board, err error) {
    notEOF := true
    if len(s.Text()) == 0 {
        notEOF = s.Scan()
    }

    i := 0
    for ; i < 5 && notEOF; i++ {
        c := s.Text()
        fmt.Printf("Read %v\n", c)
        b.board[i], err = readBoardLine(c)
        if err != nil { return }
        fmt.Printf("Read board row %v\n", b.board[i])
        notEOF = s.Scan()
    }
    if i < 5 {
        err = fmt.Errorf("Too few rows for board '%v'\n", b.board)
    } else if len(s.Text()) != 0 {
        err = fmt.Errorf("Too many rows in board %v\n", b.board)
    }

    for s.Scan() {
        if len(s.Text()) != 0 { break }
    }
    return
}


func readInput(r io.Reader) (drawn []int, boards []Board, err error) {
    s := bufio.NewScanner(r)
    if ok := s.Scan(); !ok { return }

    drawn, err = readDrawn(s)
    if err != nil { return }
    fmt.Printf("Read drawn %v\n", drawn)

    boards = make([]Board, 0)
    notEOF := true
    for notEOF {
        var board Board
        board, err = readBoard(s)
        if err != nil { return }
        fmt.Printf("Read board %v\n", board)
        boards = append(boards, board)

        if len(s.Text()) == 0 { notEOF = s.Scan() }
    }
    return
}

var marked int = -1

func markNumber(drawn int, b *Board) bool {
    fmt.Printf("Marking %d in board %v (%v %v)\n", drawn, b.board, b.markedLine, b.markedCol)
    for i := 0; i < 5; i++ {
        for j := 0; j < 5; j++  {
            if b.board[i][j] == drawn {
                fmt.Printf("Found at line %d, col %d\n", i, j)
                b.board[i][j] = marked
                b.markedCol[j]++
                b.markedLine[i]++
                if b.markedCol[j] == 5 || b.markedLine[i] == 5 {
                    return true
                }
            }
        }
    }
    return false
}

func sumBoard(b Board) (s int) {
    fmt.Printf("Summing board board %v\n", b.board)
    for i := 0; i < 5; i++ {
        for j := 0; j < 5; j++  {
            if b.board[i][j] == marked { continue }
            s += b.board[i][j]
        }
    }
    return
}

func f1 (drawn []int, bs []Board) int {
    for _, k := range drawn {
        fmt.Printf("Draw %d\n", k)
        for t := 0; t < len(bs); t++ {
            isWinner := markNumber(k, &bs[t])
            fmt.Printf("Resulting board %d: %v\n", t, bs[t])
            if isWinner {
                s := sumBoard(bs[t])
                fmt.Printf("Winner board %d: %v with sum %d\n", t, bs[t], s)
                return k * s
            }
        }
    }
    return 0
}

func RunF1(input string) {
    h0, err := os.Open(input)
    if err != nil { return }
    defer h0.Close()

    //s := bufio.NewScanner(strings.NewReader("1 2  3 4 5\n6 7 8 9 10\n11 12 13 14 15\n16 17 18 19 20\n21 22 23 24 25\n"))
    nums, boards, err := readInput(h0)
    //board, err := readBoardLine(" 1  2 3 4  5  ")
    if err != nil {
        panic(err)
    }
    fmt.Println(boards)
    x := f1(nums, boards)

    fmt.Printf("Answer1: %d\n", x)
}
