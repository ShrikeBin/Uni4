public class Node 
{
    public Integer stem;
    public Node left;
    public Node right;

    Node(Integer input) 
    {
        stem = input;
        left = right = null;
    }
}