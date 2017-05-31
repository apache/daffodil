Because the CLI tests spawn a sub-shell and exec the daffodil program
you have to 'sbt stage' first, and even then you can't set a breakpoint.

If you want to run a test in the Eclipse debugger instead of by
spawning a shell, here's the idiom.

    // tests all synthesize a command line into val cmd
    val cmd = .....
    // Instead of running the rest of the test, do these two lines:
    val args = cmd.split(' ').tail // cmdline minus the daffodil program name.
    Main.run(args) // Run Main directly, so we can debug.
 
 Note that this doesn't do the expect stuff. You have to type the
commands at the console pane, and inspect the output to see if it is
what you want.