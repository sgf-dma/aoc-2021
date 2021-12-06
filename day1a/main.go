
package main

import (
    "fmt"
    "os"
    "bufio"
    "strconv"
)

func main() {
    h0, err := os.Open("input.txt")
    if err != nil { return }
    defer h0.Close()

    s := bufio.NewScanner(h0)
    var prev, i int
    for s.Scan() {
        cur, err := strconv.Atoi(s.Text())
        if err != nil {
            fmt.Printf("%v\n", err)
            return
        }
        if prev < cur {
            i++
        }
        prev = cur
    }
    fmt.Printf("%d\n", i-1)

}
