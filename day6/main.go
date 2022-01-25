
package day6

import (
    "fmt"
    "os"
    "strconv"
    "io"
    "path/filepath"
    "bufio"
)

type Age int

func SplitComma(data []byte, atEOF bool) (advance int, token []byte, err error) {
    token = make([]byte, 0)
    i := 0
    var c rune
    //fmt.Printf("data: %q, %v\n", data, atEOF)
    out:
    for i, c = range string(data) {
        //fmt.Printf("get %q at %v\n", c, i)
        switch c {
            case ',', '\n': break out
            default: token = append(token, data[i])
        }
    }
    advance = i + 1
    //fmt.Printf("Return %q, %v\n", string(token), advance)
    return
}

func readInput(r io.Reader) (ages []Age, err error) {
    s := bufio.NewScanner(r)

    s.Split(SplitComma)
    ages = make([]Age, 0, 12)
    for s.Scan() {
        x, err := strconv.Atoi(s.Text())
        if err != nil { return nil, err }
        ages = append(ages, Age(x))
    }
    return ages, err
}

func oneDay(ages []Age) ([]Age) {
    n := 0
    for i := 0; i < len(ages); i++ {
        if ages[i] == 0 {
            n++
            ages[i] = 6
        } else {
            ages[i]--
        }
    }


    childs := make([]Age, n)
    for j := 0; j < len(childs); j++ {
        childs[j] = 8
    }
    ages = append(ages, childs...)
    return ages
}

func RunF1(input string) {
    h0, err := os.Open(filepath.Join("day6", input))
    if err != nil { return }
    defer h0.Close()

    ages, err := readInput(h0)
    /*
    for d := 0; d < 18; d++ {
        ages = oneDay(ages)
        fmt.Printf("Day %v: %v\n", d, ages)
    }*/
    for d := 0; d < 80; d++ {
        ages = oneDay(ages)
        //fmt.Printf("Day %v: %v\n", d, ages)
    }

    fmt.Printf("Answer1: %v\n", len(ages))
}
