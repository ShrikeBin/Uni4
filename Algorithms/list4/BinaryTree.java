import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public class BinaryTree<T extends Comparable<T>> 
{
    private class Node<T>
    {
        private T stem;
        private Node<T> left;
        private Node<T> right;

        Node(T input)
        {
            stem = input;
            left = right = null;
        }

        public void setLeft(Node<T> left) 
        {
            this.left = left;
        }

        public void setRight(Node<T> right) 
        {
            this.right = right;
        }

        public Node<T> getLeft() 
        {
            return left;
        }

        public Node<T> getRight() 
        {
            return right;
        }

        public T getStem() 
        {
            return stem;
        }

        public void setStem(T input)
        {
            stem = input;
        }
    }

    private Node<T> root;

    BinaryTree(Node<T> root)
    {
        this.root = root;
    }

    public Node<T> getRoot() 
    {
        return root;
    }

    public void addNode(T parentData) 
    {
        Node<T> input = new Node<T>(parentData);

        if (root == null)
        {
            this.root = input;
        }
        else
        {
            Node<T> prev = null;
            Node<T> temp = root;

            while (temp != null) 
            {
                prev = temp;
                if (temp.getStem().compareTo(parentData) > 0) 
                {
                    temp = temp.getLeft();
                }
                else if (temp.getStem().compareTo(parentData) < 0) 
                {
                    temp = temp.getRight();
                }
                else
                {
                    return; //duplicate
                }
            }
            if (prev.getStem().compareTo(parentData) > 0)
            {
                prev.setLeft(input);
            }
            else
            {
                prev.setRight(input);
            }
        }
    }

    public void deleteNode(T keyData) //deletes the Nodes of a given key
    {
        this.root = deleteAsBaseOn(this.root, keyData);
    }

    private Node<T> deleteAsBaseOn(Node<T> root, T keyData) 
    {
        if (root == null) 
        {
            return root;
        }

        if (keyData.compareTo(root.getStem()) < 0) 
        {
            root.setLeft(deleteAsBaseOn(root.getLeft(), keyData));
        }
        else if (keyData.compareTo(root.getStem()) > 0)
        {
            root.setRight(deleteAsBaseOn(root.getRight(), keyData));
        }
        else 
        {
            // Node with only one child or no child
            if (root.getLeft() == null)
            {
                return root.getRight();
            }
            else if (root.getRight() == null)
            {
                return root.getLeft();
            }

            // Node with two children
            root.setStem(smallestLeftTree(root.getRight()));
            root.setRight(deleteAsBaseOn(root.getRight(), root.getStem()));
        }

        return root;
    }


    private T smallestLeftTree(Node<T> root) 
    {
        T min = root.getStem();
        while (root.getLeft() != null) 
        {
            min = root.getLeft().getStem();
            root = root.getLeft();
        }
        return min;
    }

    //(Root - Left - Right)
    public void printTreeOrder() 
    {
        preorderTraversal(root);
        System.out.print("[END] \n");
    }

    public void printTreeLevel()
    {
        levelorderTraversal(root);
        System.out.print("[END] \n");
    }

    // (Root - Left - Right)
    private void preorderTraversal(Node<T> core) 
    {
        if (core == null)
        {
            return;
        }
        System.out.print(core.getStem() + " - ");
        preorderTraversal(core.getLeft());
        preorderTraversal(core.getRight());
    }

    // (Level, Queue printl)
    private void levelorderTraversal(Node<T> core) 
    {
        if (core == null)
        {
            return;
        }

        Queue<Node<T>> queue = new LinkedList<>();
        queue.offer(core);

        while (!queue.isEmpty()) 
        {
            Node<T> temp = queue.poll();
            System.out.print(temp.getStem() + " - ");

            if (temp.getLeft() != null)
            {
                queue.offer(temp.getLeft());
            }
            if (temp.getRight() != null)
            {
                queue.offer(temp.getRight());
            }
        }
    }

    public boolean search(T value)
    {
        return searchAsBase(root, value);
    }

    private boolean searchAsBase(Node<T> node, T value)
    {
        if (node == null)
        {
            return false;
        }

        if(value.compareTo(node.getStem()) < 0)
        {
            return searchAsBase(node.getLeft(), value);
        }
        else if(value.compareTo(node.getStem()) > 0)
        {
            return searchAsBase(node.getRight(), value);
        }
        else
        {
            return true;
        }
    }

    public List<String> getPrint()
    {
        List<String> lines = new ArrayList<>();
        printRec(root, "", true, lines);

        return lines;
    }

    private void printRec(Node<T> node, String prefix, boolean isTail, List<String> lines)
    {
        if (node == null)
        {
            return;
        }

        lines.add(prefix + (isTail ? "└── " : "├── ") + node.getStem());

        List<Node<T>> children = new ArrayList<>();
        
        if(node.getLeft() != null)
        {
            children.add(node.getLeft());
        }
        if(node.getRight() != null)
        {
            children.add(node.getRight());
        }

        for(int i = 0; i < children.size() - 1; i++)
        {
            printRec(children.get(i), prefix + (isTail ? "    " : "│   "), false, lines);
        }
        if(!children.isEmpty())
        {
            printRec(children.get(children.size() - 1), prefix + (isTail ? "    " : "│   "), true, lines);
        }
    }
}