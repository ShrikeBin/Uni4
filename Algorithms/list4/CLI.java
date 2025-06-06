import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Scanner;

public class CLI
{
    private TREE tree;

    public CLI(String type)
    {
        if(type.equals("binary"))
        {
            tree = new BinaryTree();
        }
        else if(type.equals("splay"))
        {
            tree = new SplayTree();
        }
        else if(type.equals("redblack"))
        {
            tree = new RBT();
        }
        else
        {
            throw new IllegalArgumentException("Invalid tree type");
        }
    }

    public void run()
    {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Enter commands (Ctrl+D to exit):");
        try 
        {
            while (true) 
            {
                try
                {
                    System.out.print("> ");
                    String input = scanner.nextLine().trim();

                    if (input.isEmpty()) 
                    {
                        continue;
                    }

                    String[] parts = input.split("\\s+", 2);

                    String command = parts[0];
                    String argument;
                    if (parts.length > 1) 
                    {
                        argument = parts[1];
                    } 
                    else 
                    {
                        argument = "";
                    }

                    switch (command.toLowerCase()) 
                    {
                        case "add":
                        case "a":
                        case "insert":
                        case "i":
                            tree.addNode(Integer.parseInt(argument));
                            break;

                        case "delete":
                        case "remove":
                        case "rm":
                        case "d":
                            tree.deleteNode(Integer.parseInt(argument));
                            break;

                        case "print":
                        case "display":
                        case "p":
                            if (argument.toLowerCase().equals("order")) 
                            {
                                tree.printTreeOrder();
                            } 
                            else if(argument.toLowerCase().equals("full"))
                            {
                                System.out.println("[BEGIN]");
                                tree.printTreeFull();
                                System.out.println("[END]");
                            }
                            else 
                            {
                                System.out.println("Unknown command: " + input);
                            }
                            break;
                        case "ls":
                            System.out.println("[BEGIN]");
                            tree.printTreeFull();
                            System.out.println("[END]");
                            break;

                        case "height":
                        case "h":
                        case "getheight":
                        case "geth":
                            System.out.println("Tree Height: " + tree.getHeight());
                            break; 

                        case "ex":
                            int limit = Integer.parseInt(argument);
                            for(int i = limit/2; i < limit; i++)
                            {
                                tree.addNode(i);
                            }
                            for(int i = limit/2; i >= 0; i--)
                            {
                                tree.addNode(i);
                            }
                            break;
                        
                        case "random":
                            int n = Integer.parseInt(argument);
                            Random rand = new Random();
                            for (int i = 0; i < n; i++) {
                                tree.addNode(rand.nextInt(2 * n));
                            }
                            break;
                        case "exit":
                            throw new NoSuchElementException("exiting");

                        default:
                            System.out.println("Unknown command: " + input);
                            break;
                    }
                }
                catch (IllegalArgumentException e)
                {
                    System.err.println("Invalid argument");
                } 
            }   
        } 
        catch (NoSuchElementException e) 
        {
            System.out.println("closing...");
        }
        finally 
        {
            scanner.close();
        }
    }
}