
package day5

import (
    "fmt"
    "os"
    "strconv"
    "io"
    "bufio"
    "strings"
)

type Matrix struct {
    data [][]int
    rows, cols int
}

func (m *Matrix) addRows(r int) {
    if r <= 0 { return }

    m.rows += r
    if m.cols == 0 { return }

    for i := 0; i < r; i++ {
        fill := make([]int, m.cols)
        m.data = append(m.data, fill)
    }
}

func (m *Matrix) addCols(c int) {
    if c <= 0 { return }

    m.cols += c
    if m.rows == 0 { return }

    fill := make([]int, c)
    for y := 0; y < m.rows; y++ {
        m.data[y] = append(m.data[y], fill...)
    }
}

func (m *Matrix) hasPoint(p Point) {
    //fmt.Printf("Matrix\n%v\nhas point %v?\n", m, p)
    if p.x >= m.cols {
        m.addCols(p.x + 1 - m.cols)
    }
    if p.y >= m.rows {
        m.addRows(p.y + 1 - m.rows)
    }
    //fmt.Printf("Resulting matrix\n%v\n", m)
}

func (m Matrix) String() (s string) {
    for y := 0; y < m.rows; y++ {
        s += "\n"
        for x := 0; x < m.cols; x++ {
            v := m.data[y][x]
            if v == 0 {
                s += "."
            } else {
                s += strconv.Itoa(v)
            }
        }
    }
    return
}

type Point struct {
    x, y int
}

type Line struct {
    start, end Point
}

func readInput(r io.Reader) (lines []Line, err error) {
    s := bufio.NewScanner(r)

    lines = make([]Line, 0, 12)
    for s.Scan() {
        ws := strings.Split(s.Text(), " ")
        ls := strings.Split(ws[0], ",")
        rs := strings.Split(ws[2], ",")
        x1, err := strconv.Atoi(ls[0])
        if err != nil { return nil, err }
        y1, err := strconv.Atoi(ls[1])
        if err != nil { return nil, err }
        x2, err := strconv.Atoi(rs[0])
        if err != nil { return nil, err }
        y2, err := strconv.Atoi(rs[1])
        if err != nil { return nil, err }

        p := Line {start: Point{x1, y1}, end: Point{x2, y2}}
        lines = append(lines, p)
    }
    return lines, err
}

func max(x, y int) (v int){
    v = x
    if x < y { v = y }
    return
}

func min(x, y int) (v int){
    v = x
    if x > y { v = y }
    return
}

func (m *Matrix) drawLines(lines []Line) {
    for _, l := range lines {
        if l.start.x == l.end.x {
            //fmt.Printf("Draw vertical line %v on\n%v\n", l, m)

            m.hasPoint(l.start)
            m.hasPoint(l.end)

            x := l.start.x
            y := min(l.start.y, l.end.y)
            for ; y <= max(l.start.y, l.end.y); y++ {
                //fmt.Printf("draw [%v][%v]\n", x, y)
                m.data[y][x]++
                //fmt.Printf("Matrix\n%v\n", m)
            }
            //fmt.Printf("Drawn \n%v\n", m)
        } else if l.start.y == l.end.y {
            //fmt.Printf("Draw horizontal line %v on\n%v\n", l, m)

            m.hasPoint(l.start)
            m.hasPoint(l.end)

            x := min(l.start.x, l.end.x)
            y := l.start.y
            for ; x <= max(l.start.x, l.end.x); x++ {
                m.data[y][x]++
            }
        }
    }
}

func f1(lines []Line) (count int) {
    var m Matrix

    m.drawLines(lines)
    for y := 0; y < m.rows; y++ {
        for x := 0; x < m.cols; x++ {
            if m.data[y][x] > 1 { count++ }
        }
    }
    return count
}

func RunF1(input string) {
    h0, err := os.Open(input)
    if err != nil { return }
    defer h0.Close()

    /*
    var n Matrix
    //n.hasPoint(Point{2, 2})
    fmt.Printf("%v\n", n)
    n.addCols(3)
    n.addRows(3)
    fmt.Printf("%v\n", n)
    n.data[0][0] = 1
    n.data[0][1] = 2
    n.data[0][2] = 3
    n.data[1][0] = 4
    n.data[1][1] = 5
    n.data[1][2] = 6
    n.data[2][0] = 7
    n.data[2][1] = 8
    n.data[2][2] = 9
    fmt.Printf("%v\n", n)
    */

    ls, err := readInput(h0)
    fmt.Printf("Input %v\n", ls)

    var m Matrix
    m.drawLines(ls)
    count := f1(ls)
    //fmt.Printf("Matrix %v\n", m)
    fmt.Printf("Answer1: %v\n", count)
}

