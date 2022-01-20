
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

func abs(x float64) float64 {
    if x < 0  { return -x }
    return x
}

func (m *Matrix) drawLines(lines []Line) {
    for _, l := range lines {
        if l.start.x == l.end.x {
            //fmt.Printf("Draw vertical line %v on %v\n", l, m)

            m.hasPoint(l.start)
            m.hasPoint(l.end)

            x := l.start.x
            y := min(l.start.y, l.end.y)
            for ; y <= max(l.start.y, l.end.y); y++ {
                //fmt.Printf("draw [%v][%v]\n", x, y)
                m.data[y][x]++
                //fmt.Printf("Matrix %v\n", m)
            }
            //fmt.Printf("Drawn \n%v\n", m)
        } else if l.start.y == l.end.y {
            //fmt.Printf("Draw horizontal line %v on %v\n", l, m)

            m.hasPoint(l.start)
            m.hasPoint(l.end)

            x := min(l.start.x, l.end.x)
            y := l.start.y
            for ; x <= max(l.start.x, l.end.x); x++ {
                m.data[y][x]++
            }
        } else {
            //fmt.Printf("Skip point %v\n", l)
        }
    }
}

func (m *Matrix) drawLines2(lines []Line) {
    for _, l := range lines {
        if l.start.x == l.end.x {
            //fmt.Printf("Draw vertical line %v on %v\n", l, m)

            m.hasPoint(l.start)
            m.hasPoint(l.end)

            x := l.start.x
            y := min(l.start.y, l.end.y)
            for ; y <= max(l.start.y, l.end.y); y++ {
                //fmt.Printf("draw [%v][%v]\n", x, y)
                m.data[y][x]++
                //fmt.Printf("Matrix %v\n", m)
            }
            //fmt.Printf("Drawn \n%v\n", m)
        }

        kf := float64(l.end.y - l.start.y) / float64(l.end.x - l.start.x)
        //fmt.Printf("Line %v (k = %v)\n", l, kf)

        if abs(kf) == 1 || kf == 0 {

            m.hasPoint(l.start)
            m.hasPoint(l.end)

            k := int(kf)
            b := l.start.y - k * l.start.x
            //fmt.Printf("Draw line %v (k = %v, b = %v)\n", l, k, b)
            //fmt.Printf("Over matrix %v\n", m)
            for x := min(l.start.x, l.end.x); x <= max(l.start.x, l.end.x); x++ {
                y := k*x + b
                //fmt.Printf("draw [%v][%v]\n", x, y)

                m.data[y][x]++
                //fmt.Printf("Matrix %v\n", m)
            }
            //fmt.Printf("Drawn matrix %v\n", m)
        } else {
            //fmt.Printf("Skipping line %v\n", l)
        }
    }
}

func f1(lines []Line) (count int) {
    var m Matrix

    m.drawLines(lines)
    fmt.Printf("rows %v, cols %v\n", m.rows, m.cols)
    for y := 0; y < m.rows; y++ {
        for x := 0; x < m.cols; x++ {
            if m.data[y][x] > 1 { count++ }
        }
    }
    return count
}

func f2(lines []Line) (count int) {
    var m Matrix

    m.drawLines2(lines)
    fmt.Printf("rows %v, cols %v\n", m.rows, m.cols)
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

    count := f1(ls)
    //fmt.Printf("Matrix %v\n", m)
    fmt.Printf("Answer1: %v\n", count)
}

func RunF2(input string) {
    h0, err := os.Open(input)
    if err != nil { return }
    defer h0.Close()

    ls, err := readInput(h0)
    fmt.Printf("Input %v\n", ls)

    count := f2(ls)
    //fmt.Printf("Matrix %v\n", m)
    fmt.Printf("Answer2: %v\n", count)
}
