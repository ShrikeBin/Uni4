import java.util.NoSuchElementException;
import java.util.Scanner;

public class CLI<T extends Comparable<T>> 
{
    private Parser<T> parser;
    private BinaryTree<T> tree;

    public CLI(Parser<T> parser)
    {
        this.parser = parser;
        tree = new BinaryTree<T>(null);
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
                    String input = scanner.nextLine().trim(); // trim any spaces

                    if (input.isEmpty()) 
                    {
                        continue; // Skip empty lines
                    }

                    String[] parts = input.split("\\s+", 2); // Split into command and argument(s)

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
                            tree.addNode(parser.parse(argument));
                            break;

                        case "delete":
                        case "remove":
                        case "rm":
                            tree.deleteNode(parser.parse(argument));
                            break;

                        case "print":
                            if (argument.toLowerCase().equals("order")) 
                            {
                                tree.printTreeOrder();
                            } 
                            else if (argument.toLowerCase().equals("level")) 
                            {
                                tree.printTreeLevel();
                            } 
                            else if(argument.toLowerCase().equals("full"))
                            {
                                System.out.println("[BEGIN]");
                                for(String line : tree.getPrint())
                                {
                                    System.out.println(line);
                                }
                            }
                            else 
                            {
                                System.out.println("Unknown command: " + input);
                            }
                            break;

                        case "search":
                            System.out.println("Result: " + tree.search(parser.parse(argument)));
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