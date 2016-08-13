// +build enums

package main

import (
	"encoding/json"
	"fmt"
)

type PageSize uint8
type PageFont uint8

const (
	A3 PageSize = iota + 1 // create an enum, starting from 1
	A4
	A5
)

const (
	ARIAL PageFont = iota + 1
	ROBOTO
	FUTURA
)

type Page struct {
	Content string   `json:"content"`
	Size    PageSize `json:"size"`
	Font    PageFont `json:"font"`
}

func main() {
	page1 := Page{
		Content: "Lorem ipsum",
		Size:    A4,
		Font:    ARIAL,
	}

	v, _ := json.Marshal(page1)
	fmt.Println(string(v)) // => {"content":"Lorem ipsum","size":2,"font":1}

	page1.Size = A3

	v, _ = json.Marshal(page1)
	fmt.Println(string(v)) // => {"content":"Lorem ipsum","size":1,"font":1}

	// This gives error: "cannot use FUTURA (type PageFont) as type PageSize in assignment"
	// page1.Size = FUTURA
}
