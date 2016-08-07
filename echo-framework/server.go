package main

import (
	"net/http"
	"strconv"

	"github.com/labstack/echo"
	"github.com/labstack/echo/engine/standard"
)

// User with a name and age
type User struct {
	ID   int
	Name string `json:"name" xml:"name" form:"name"`
	Age  int    `json:"age" xml:"age" form:"age"`
}

var (
	users     = map[int]*User{}
	userCount = 0
)

func main() {
	e := echo.New()

	saveUser := func(c echo.Context) error {
		user := &User{ID: userCount}
		if e := c.Bind(user); e != nil {
			return e
		}
		users[user.ID] = user
		userCount++
		return c.JSON(http.StatusCreated, user)
	}

	getUser := func(c echo.Context) error {
		id, _ := strconv.Atoi(c.Param("id"))
		if user, present := users[id]; present {
			return c.JSON(http.StatusOK, user)
		}
		return c.JSON(http.StatusNotFound, nil)
	}

	updateUser := func(c echo.Context) error {
		user := &User{}
		if e := c.Bind(user); e != nil {
			return e
		}

		id, _ := strconv.Atoi(c.Param("id"))
		if user, present := users[id]; present {
			user.ID = id
			users[id] = user
			return c.JSON(http.StatusOK, user)
		}

		return c.JSON(http.StatusNotFound, nil)
	}

	deleteUser := func(c echo.Context) error {
		id, _ := strconv.Atoi(c.Param("id"))
		if _, present := users[id]; present {
			delete(users, id)
			return c.NoContent(http.StatusNoContent)
		}

		return c.JSON(http.StatusNotFound, nil)
	}

	e.POST("/users", saveUser)
	e.GET("/users/:id", getUser)
	e.PUT("/users/:id", updateUser)
	e.DELETE("/users/:id", deleteUser)

	e.Run(standard.New(":8080"))
}
