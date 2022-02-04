
package day6

import (
    "fmt"
    "os"
    "strconv"
    "io"
    "path/filepath"
    "bufio"
)

type BirthTimer int

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

func readInput(r io.Reader) (ages []BirthTimer, err error) {
    s := bufio.NewScanner(r)

    s.Split(SplitComma)
    ages = make([]BirthTimer, 0, 12)
    for s.Scan() {
        x, err := strconv.Atoi(s.Text())
        if err != nil { return nil, err }
        ages = append(ages, BirthTimer(x))
    }
    return ages, err
}

func oneDay(ages []BirthTimer) ([]BirthTimer) {
    n := 0
    for i := 0; i < len(ages); i++ {
        if ages[i] == 0 {
            n++
            ages[i] = 6
        } else {
            ages[i]--
        }
    }


    childs := make([]BirthTimer, n)
    for j := 0; j < len(childs); j++ {
        childs[j] = 8
    }
    ages = append(ages, childs...)
    return ages
}

func RunF1Days(input string, days int) int {
    h0, err := os.Open(filepath.Join("day6", input))
    if err != nil { return 0 }
    defer h0.Close()

    ages, err := readInput(h0)
    /*
    for d := 0; d < 18; d++ {
        ages = oneDay(ages)
        fmt.Printf("Day %v: %v\n", d, ages)
    }*/
    //for d := 0; d < 80; d++ {
    for d := 0; d < days; d++ {
        ages = oneDay(ages)
        //fmt.Printf("Day %v: %v\n", d, ages)
    }

    return len(ages)
}

func RunF1 (input string) {
    n := RunF1Days(input, 80)
    fmt.Printf("Answer1: %v\n", n)
}

type Fish struct {
    age int
    childs []Fish
}

func readInput2(r io.Reader) (fishes []Fish, err error) {
    s := bufio.NewScanner(r)

    s.Split(SplitComma)
    fishes = make([]Fish, 0, 12)
    for s.Scan() {
        x, err := strconv.Atoi(s.Text())
        if err != nil { return nil, err }
        fishes = append(fishes, Fish{age: 8 - x})
    }
    return fishes, err
}

func live(fish *Fish) {
    if fish.age < 9 {
        return
    }
    //fmt.Printf("   Fish: %v\n", fish)
    fish.age -= 2
    for fish.age >= 7 {
        fish.age -= 7
        fish.childs = append(fish.childs, Fish{age: fish.age})
        //fmt.Printf("   Fish child: %v\n", fish)
    }
}

func liveDescendants(fish *Fish) {
    live(fish)
    //fmt.Printf("Fish lived: %v\n", fish)
    for i := 0; i < len(fish.childs); i++ {
        //fmt.Printf("Living descendant %d\n", i)
        liveDescendants(&fish.childs[i])
    }
}

func countNodes(fish Fish) int {
    n := 1
    for i := range fish.childs {
        n += countNodes(fish.childs[i])
    }
    return n
}

func RunF2Days(input string, days int) int {
    h0, err := os.Open(filepath.Join("day6", input))
    if err != nil { return 0 }
    defer h0.Close()

    fishes, err := readInput2(h0)
    //fmt.Printf("Starting with fishes: %v\n", fishes)
    n := 0
    for _, x := range fishes {
        //fmt.Printf("### Live top-level fish %v\n", x)
        x.age = x.age + days
        liveDescendants(&x)
        n += countNodes(x)
    }
    //fmt.Printf("Nodes: %v\n", n)
    return n
}

func RunF2(input string) {
    n := RunF2Days(input, 80)
    fmt.Printf("Nodes: %v\n", n)
}
