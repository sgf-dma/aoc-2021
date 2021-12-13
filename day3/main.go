
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

func parseDiag(r io.Reader) ([]BitCount, []uint32, error) {
    s := bufio.NewScanner(r)

    if ok := s.Scan(); !ok { return nil, nil, nil }

    line := s.Text()
    n := len(line)
    bc := make([]BitCount, n)
    nums := make([]uint32, 0, 1)

    for {
        //fmt.Printf("%v\n", line)
        var x uint32 = 0
        for i, c := range line {
            k := n - 1 -i
            switch c {
                case '0':
                    bc[k].zero++
                case '1':
                    bc[k].one++
                    x |= 1<<k
                default:
                    return nil, nil, fmt.Errorf("Wrong input")
            }
        }
        //fmt.Printf("%d, %0b\n", x, x)
        //fmt.Printf("%v\n", bc)
        nums = append(nums, x)
        if ok := s.Scan(); !ok { break }
        line = s.Text()
    }
    return bc, nums, nil
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

    bc, nums, err := parseDiag(h0)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }
    fmt.Printf("bc=%v, nums=%v\n", bc, nums)
    gamma, epsilon := f1(bc)
    //findSimilar(gamma, nums)
    //fmt.Printf("%0b\n", (^t<<27)>>27)

    fmt.Printf("Answer1: %d (%d(%0b), %d(%0b))\n", gamma * epsilon, gamma, gamma, epsilon, epsilon)
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
    fmt.Printf("Group by bit %d with mask %d\n", bc.bit, mask)
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

func f2 (maxBit int, allNums []uint32) (oxygen uint32, co2 uint32) {

    nums := allNums
    for i := maxBit - 1; i >= 0 && len(nums) > 1; i-- {
        //fmt.Printf("Grouping numbers %0b\n", nums)
        bg := groupByBit(uint32(i), nums)
        if len(bg.ones) >= len(bg.zeros) {
            nums = bg.ones
        } else {
            nums = bg.zeros
        }
        //fmt.Printf("Groups by bit 2^%d are 1=%0b(%d) and 0=%0b(%d)\n", i, bg.ones, len(bg.ones), bg.zeros, len(bg.zeros))
    }
    if len(nums) == 1 {
        oxygen = nums[0]
        fmt.Printf("Found number %d (%0b)\n", nums[0], nums[0])
    } else {
        panic("Impossible, more than one oxygen number left")
    }

    nums = allNums
    for i := maxBit - 1; i >= 0 && len(nums) > 1; i-- {
        fmt.Printf("Grouping numbers %0b\n", nums)
        bg := groupByBit(uint32(i), nums)
        if len(bg.ones) >= len(bg.zeros) {
            nums = bg.zeros
        } else {
            nums = bg.ones
        }
        fmt.Printf("Groups by bit 2^%d are 1=%0b(%d) and 0=%0b(%d)\n",
            i, bg.ones, len(bg.ones), bg.zeros, len(bg.zeros))
    }
    fmt.Printf("Left %0b\n", nums)
    if len(nums) == 1 {
        co2 = nums[0]
        fmt.Printf("Found number %d (%0b)\n", nums[0], nums[0])
    } else {
        panic("Impossible, more than one co2 number left")
    }

    return
}

func RunF2(input string) {
    h0, err := os.Open(input)
    if err != nil { return }
    defer h0.Close()

    bc, nums, err := parseDiag(h0)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }
    fmt.Printf("bc=%v, nums=%v (%d)\n", bc, nums, len(nums))
    oxygen, co2 := f2(len(bc), nums)
    fmt.Printf("Answer1: %d (%d, %d)\n", oxygen * co2, oxygen, co2)

}
