package main

import "fmt"

type myError struct {
	code int
	msg  string
}

func (m *myError) Error() string {
	return fmt.Sprintf("Error! Code: %d, Message: %s", m.code, m.msg)
}

func erroneousDoubler(a int) (int, error) {
	if a == 0 {
		return -1, &myError{code: 400, msg: "Dude don't try to multiply 0 by anything."}
	}
	return a * 2, nil
}
func main() {
	for i := -3; i < 2; i++ {
		if r, e := erroneousDoubler(i); e == nil {
			fmt.Printf("Result: %d\n", r)
		} else {
			fmt.Printf("Error: %s\n", e)
		}
	}

	fmt.Println()

	_, e := erroneousDoubler(0)
	// Type assertion. e.(*myError) asserts e into a myError ptr
	// Type conversion won't work since they are unrelated types
	if assertedError, ok := e.(*myError); ok {
		fmt.Println("Code:", assertedError.code)
		fmt.Println("Message:", assertedError.msg)
	}

	// Type switch. Like type assertion, but uses `.(type)` syntax
	var i interface{} = "Wow"
	switch t := i.(type) {
	case int:
		fmt.Printf("It's an integer: %d\n", t)
	case string:
		fmt.Printf("It's a string: %s\n", t)
	case bool:
		fmt.Printf("It's a bool: %t\n", t)
	default:
		fmt.Printf("It's superman! type: %T\n", t) // %T prints the type
	}
}
