
package main

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

func main() {
    h0, err := os.Open("input.txt")
    if err != nil { return }
    defer h0.Close()

    i, err := f1(h0)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }
    fmt.Printf("%d\n", i)

}
