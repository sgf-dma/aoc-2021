
package day3

import (
    "fmt"
    "os"
    "io"
    "bufio"
    //"strings"
    //"strconv"
)

type BitCount struct {
    zero, one int
}

/*
func CountBits (x int) (bc []BitCount) {
    for i := 0; i < 32; i++ {
        if x & 1<<i == 1 { bc.zero++ }
    }
}
*/

func parseDiag(r io.Reader) ([]BitCount, error) {
    s := bufio.NewScanner(r)

    if ok := s.Scan(); !ok { return nil, nil }

    line := s.Text()
    n := len(line)
    bc := make([]BitCount, n)

    for {
        //fmt.Printf("%v\n", line)
        for i, c := range line {
            switch c {
                case '0':
                    bc[n-1-i].zero++
                case '1':
                    bc[n-1-i].one++
                default:
                    return nil, fmt.Errorf("Wrong input")
            }
        }
        //fmt.Printf("%v\n", bc)
        if ok := s.Scan(); !ok { break }
        line = s.Text()
    }
    return bc, nil
}

func f1(bc []BitCount) (gamma uint32, epsilon uint32) {
    n := len(bc)
    for i := 0; i < n; i++ {
        //fmt.Printf("bc=%#v\n", bc[i])
        if bc[i].one > bc[i].zero {
            //fmt.Printf("one at %d\n", i)
            gamma |= 1<<i
        }
    }
    t := 32 - n
    // FIXME: Does uint rquired?
    epsilon = (^gamma<<t)>>t
    return
}

func RunF1(input string) {
    h0, err := os.Open(input)
    if err != nil { return }
    defer h0.Close()

    bc, err := parseDiag(h0)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }
    //fmt.Printf("bc=%#v\n", bc)
    gamma, epsilon := f1(bc)
    //fmt.Printf("%0b\n", (^t<<27)>>27)

    fmt.Printf("Answer1: %d (%d, %d)\n", gamma * epsilon, gamma, epsilon)
}

