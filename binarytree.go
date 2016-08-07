package binaryTree

import "fmt"

// New empty binary tree
func New() *BinaryTree {
	return new(BinaryTree)
}

type binaryNode struct {
	data       int
	leftChild  *binaryNode
	rightChild *binaryNode
}

// BinaryTree is a simple binary tree
type BinaryTree struct {
	root *binaryNode
	size int
}

// Add adds a new element to the tree and returns true if it's successful
func (b *BinaryTree) Add(i int) bool {
	node := b.root
	parent := b.root
	left := true
	newNode := &binaryNode{data: i}

	// empty tree case
	if node == nil {
		b.root = newNode
		b.size++
		return true
	}

	for node != nil {
		if i < node.data {
			parent = node
			node = node.leftChild
			left = true
		} else if i > node.data {
			parent = node
			node = node.rightChild
			left = false
		} else {
			return false // val exists
		}
	}

	if left {
		parent.leftChild = newNode
	} else {
		parent.rightChild = newNode
	}

	b.size++
	return true
}

// RecursiveAdd adds a new element to the tree and returns true if it's successful
func (b *BinaryTree) RecursiveAdd(i int) bool {
	initialSize := b.size

	b.root = b.recursiveAdd(i, b.root)

	// element is not added
	if initialSize == b.size {
		return false
	}

	return true
}

func (b *BinaryTree) recursiveAdd(i int, node *binaryNode) *binaryNode {
	if node == nil {
		b.size++
		return &binaryNode{data: i}
	}

	if i < node.data {
		node.leftChild = b.recursiveAdd(i, node.leftChild)
	} else if i > node.data {
		node.rightChild = b.recursiveAdd(i, node.rightChild)
	}

	// Element exists case and recursion intermediate step case
	return node

}

// Size returns the number of elements currently the binary tree has
func (b *BinaryTree) Size() int {
	return b.size
}

// Show shows the tree's current status
func (b *BinaryTree) Show() {
	b.traverseInOrder(b.root)
}

func (b *BinaryTree) traverseInOrder(root *binaryNode) {
	if root == nil {
		return
	}
	b.traverseInOrder(root.leftChild)
	fmt.Print(root.data, " ")
	b.traverseInOrder(root.rightChild)
}

func (b *BinaryTree) traversePreOrder(root *binaryNode) {
	if root == nil {
		return
	}
	fmt.Print(root.data, " ")
	b.traversePreOrder(root.leftChild)
	b.traversePreOrder(root.rightChild)
}

func (b *BinaryTree) traversePostOrder(root *binaryNode) {
	if root == nil {
		return
	}
	b.traversePostOrder(root.leftChild)
	b.traversePostOrder(root.rightChild)
	fmt.Print(root.data, " ")
}
