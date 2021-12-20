
package day3

import (
    "fmt"
    "os"
    "io"
    "bufio"
    //"strings"
)

type BitCount struct {
    zero, one int
}

func parseDiag(r io.Reader) ([]BitCount, error) {
    s := bufio.NewScanner(r)

    if ok := s.Scan(); !ok { return nil, nil }

    line := s.Text()
    n := len(line)
    bc := make([]BitCount, n)

    for {
        //fmt.Printf("%v\n", line)
        for i, c := range line {
            k := n - 1 -i
            switch c {
                case '0':
                    bc[k].zero++
                case '1':
                    bc[k].one++
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
    //fmt.Printf("bc=%v\n", bc)
    gamma, epsilon := f1(bc)
    //findSimilar(gamma, nums)
    //fmt.Printf("%0b\n", (^t<<27)>>27)

    fmt.Printf("Answer1: %d (%d(%0b), %d(%0b))\n", gamma * epsilon, gamma, gamma, epsilon, epsilon)
}

func readDiag(r io.Reader) (nums []uint32, maxBit uint32, err error) {
    s := bufio.NewScanner(r)

    if ok := s.Scan(); !ok { return }

    line := s.Text()
    maxBit = uint32(len(line))
    nums = make([]uint32, 0, 1)

    for {
        //fmt.Printf("%v\n", line)
        var x uint32 = 0
        for i, c := range line {
            k := int(maxBit) - 1 - i
            switch c {
                case '0': continue
                case '1': x |= 1<<k
                default:
                    err = fmt.Errorf("Wrong input")
                    return
            }
        }
        //fmt.Printf("%d, %0b\n", x, x)
        //fmt.Printf("%v\n", bc)
        nums = append(nums, x)
        if ok := s.Scan(); !ok { break }
        line = s.Text()
    }
    return
}

type BitGroup struct {
    bit uint32
    ones []uint32
    zeros []uint32
}

func groupByBit (bit uint32, nums []uint32) (bc BitGroup) {
    bc.bit = bit
    bc.ones = make([]uint32, 0)
    bc.zeros = make([]uint32, 0)
    var mask uint32 = 1<<bc.bit
    //fmt.Printf("Group by bit %d with mask %d\n", bc.bit, mask)
    for _, x := range nums {
        //fmt.Printf("Num %d\n", x)
        if x&mask != 0 {
            bc.ones = append(bc.ones, x)
        } else {
            bc.zeros = append(bc.zeros, x)
        }
    }
    return
}

func findNumber (maxBit uint32, nums []uint32, choose func(BitGroup) []uint32) uint32 {
    for i := maxBit - 1; i >= 0 && len(nums) > 1; i-- {
        //fmt.Printf("Grouping numbers %0b\n", nums)
        bg := groupByBit(i, nums)
        nums = choose(bg)
        //fmt.Printf("Groups by bit 2^%d are 1=%0b(%d) and 0=%0b(%d)\n", i, bg.ones, len(bg.ones), bg.zeros, len(bg.zeros))
    }

    if len(nums) == 1 {
        //fmt.Printf("Found number %d (%0b)\n", nums[0], nums[0])
        return nums[0]
    }
    panic("Impossible, more than one number left.")
}

func f2 (nums []uint32, maxBit uint32) (oxygen uint32, co2 uint32) {
    mostCommon := func (gr BitGroup) []uint32 {
        if len(gr.ones) >= len(gr.zeros) {
            return gr.ones
        }
        return gr.zeros
    }
    oxygen = findNumber(maxBit, nums, mostCommon)

    leastCommon := func (gr BitGroup) []uint32 {
        if len(gr.ones) >= len(gr.zeros) {
            return gr.zeros
        }
        return gr.ones
    }
    co2 = findNumber(maxBit, nums, leastCommon)

    return
}

func RunF2(input string) {
    h0, err := os.Open(input)
    if err != nil { return }
    defer h0.Close()

    nums, maxBit, err := readDiag(h0)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }
    //fmt.Printf("bc=%v, nums=%v (%d)\n", bc, nums, len(nums))
    oxygen, co2 := f2(nums, maxBit)
    fmt.Printf("Answer2: %d (%d, %d)\n", oxygen * co2, oxygen, co2)

}

