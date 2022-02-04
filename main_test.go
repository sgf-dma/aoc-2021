
package main

import (
    "testing"
    "sgf-dma/aoc-2021/day6"
)

func BenchmarkRunF1Days(b *testing.B) {
    for i := 0; i < b.N; i++ {
        day6.RunF1Days("in.txt", 100)
    }
}

func BenchmarkRunF2Days(b *testing.B) {
    for i := 0; i < b.N; i++ {
        day6.RunF2Days("in.txt", 100)
    }
}
