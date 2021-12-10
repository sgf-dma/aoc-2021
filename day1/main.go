
package day1

import (
    "fmt"
    "os"
    "bufio"
    "strconv"
    "io"
)

func f1 (r io.Reader) (int, error) {
    s := bufio.NewScanner(r)
    var prev, i int
    for s.Scan() {
        cur, err := strconv.Atoi(s.Text())
        if err != nil {
            return 0, fmt.Errorf("%v\n", err)
        }
        if prev < cur {
            i++
        }
        prev = cur
    }
    return i-1, nil
}

func fillBuffer(s *bufio.Scanner, buf []int) (int, error) {
    i := 0
    for s.Scan() {
        x, err := strconv.Atoi(s.Text())
        if err != nil {
            return 0, fmt.Errorf("%v\n", err)
        }
        buf[i] = x
        i++
        if i >= len(buf) { break }
    }
    return i, nil
}

func sum(xs []int) (r int) {
    for i := 0; i < len(xs); i++ {
        r += xs[i]
    }
    return r
}

func compareBuffer(buf []int) (z int) {
    //fmt.Printf("Compare buffer %v (%d)\n", buf, len(buf))
    for j := 4; j <= len(buf); j++ {
        x := buf[j-4:j-1]
        y := buf[j-3:j]
        //fmt.Printf("Compare %v (%d) with %v (%d) at %d\n", x, sum(x), y, sum(y), j)
        if sum(x) < sum(y) {
            z++
        }
    }
    return
}

func f2 (r io.Reader) (int, error) {
    s := bufio.NewScanner(r)
    var buf []int = make([]int, 1024)
    z := 0
    k := 0
    for {
        n, err := fillBuffer(s, buf[k:])
        if err != nil {
            return 0, fmt.Errorf("%v\n", err)
        }
        if n == 0 { break }
        z += compareBuffer(buf[:k+n])
        //fmt.Printf("Current increases %d\n", z)

        copy(buf, buf[n+k-3:])
        k = 3
    }
    return z, nil
}

func RunF1(input string) {
    h0, err := os.Open(input)
    if err != nil { return }
    defer h0.Close()

    i, err := f1(h0)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }
    fmt.Printf("Answer: %d\n", i)
}

func RunF2(input string) {
    h0, err := os.Open(input)
    if err != nil { return }
    defer h0.Close()

    n, err := f2(h0)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }
    fmt.Printf("Answer: %d\n", n)
}

