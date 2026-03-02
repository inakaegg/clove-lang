package main

import (
    "fmt"
    "sort"
    "strings"
)

func interleave(left []int64, right []int64) []int64 {
    length := len(left)
    if len(right) < length {
        length = len(right)
    }
    out := make([]int64, 0, length*2)
    for i := 0; i < length; i++ {
        out = append(out, left[i], right[i])
    }
    return out
}

func main() {
    repsFast := int64(3000000)
    repsMid := int64(400000)
    repsSlow := int64(200)

    // map / filter / reduce
    var sumMap int64 = 0
    var sumFilter int64 = 0
    for i := int64(0); i < repsFast; i++ {
        sumMap += i + 1
        if i%2 == 0 {
            sumFilter += i
        }
    }

    // hash-map / conj / nth / rest / interleave / count / empty?
    var sumCollections int64 = 0
    for x := int64(0); x < repsMid; x++ {
        m := map[string]int64{"a": x, "b": x}
        keys := make([]string, 0, len(m))
        for k := range m {
            keys = append(keys, k)
        }
        sort.Strings(keys)
        v := []int64{x, x}
        n0 := v[0]
        r := append([]int64{}, v[1:]...)
        i := interleave(v, r)
        c := int64(len(i))
        if len(m) == 0 {
            sumCollections += 0
        } else {
            sumCollections += c + n0
        }
    }

    // remove / drop-while / keep / keep-indexed
    var sumKeep int64 = 0
    idx := int64(0)
    for i := int64(0); i < repsMid; i++ {
        if i%2 == 0 && i >= 10 {
            if idx%2 == 0 {
                sumKeep += i
            }
            idx++
        }
    }

    // sort / sort-by / some
    small := make([]int, 20000)
    for i := 0; i < len(small); i++ {
        small[i] = i
    }
    var sumSort int64 = 0
    for i := int64(0); i < repsSlow; i++ {
        sorted := append([]int(nil), small...)
        sort.Ints(sorted)
        sortby := append([]int(nil), small...)
        sort.Slice(sortby, func(i, j int) bool { return sortby[i] > sortby[j] })
        someFlag := int64(0)
        for _, v := range small {
            if v%2 == 0 {
                someFlag = 1
                break
            }
        }
        sumSort += int64(len(sorted)+len(sortby)) + someFlag
    }

    // split / join
    text := "a,b,c,d,e,f,g,h,i,j"
    var sumText int64 = 0
    for i := int64(0); i < repsMid; i++ {
        parts := strings.Split(text, ",")
        joined := strings.Join(parts, "|")
        sumText += int64(len(joined) + len(parts))
    }

    total := sumMap + sumFilter + sumCollections + sumKeep + sumSort + sumText
    fmt.Println(total)
}
