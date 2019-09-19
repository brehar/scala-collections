package object collections {
  type Stack[+E] = List[E]
  @inline val Stack: List.type = List
}
