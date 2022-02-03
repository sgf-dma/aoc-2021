
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
    if fish.age < 8 {
        return
    }
    fmt.Printf("   Fish: %v\n", fish)
    fish.age -= 1
    for fish.age >= 7 {
        fish.age -= 7
        fish.childs = append(fish.childs, Fish{age: fish.age})
        fmt.Printf("   Fish child: %v\n", fish)
    }
}

func liveDescendants(fish *Fish) {
    live(fish)
    fmt.Printf("Fish lived: %v\n", fish)
    for i := 0; i < len(fish.childs); i++ {
        fmt.Printf("Living descendant %d\n", i)
        liveDescendants(&fish.childs[i])
    }
}

func calculateAge(bt BirthTimer, days int) int {
    return 8 - int(bt) + days
}

func RunF2(input string) {
    h0, err := os.Open(filepath.Join("day6", input))
    if err != nil { return }
    defer h0.Close()

    fishes, err := readInput2(h0)
    fmt.Printf("Starting with fishes: %v\n", fishes)
    for _, x := range fishes[:1] {
        fmt.Printf("### Live top-level fish %v\n", x)
        x.age = x.age + 9
        liveDescendants(&x)
    }
    /*
    for d := 0; d < 18; d++ {
        ages = oneDay(ages)
        fmt.Printf("Day %v: %v\n", d, ages)
    }*/
    /*
    for d := 0; d < 18; d++ {
        ages = oneDay(ages)
        //fmt.Printf("Day %v: %v\n", d, ages)
    }*/

    //fmt.Printf("Answer2: %v\n", len(ages))
}
