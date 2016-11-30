from invoke import task


@task
def install(ctx):
    """Install the ratchets R package."""
    R_commands = """
    library(devtools)
    pkg <- "ratchets"
    document(pkg)
    install(pkg)
    """.split()
    ctx.run('Rscript -e "{}"'.format(';'.join(R_commands)))


@task
def compile(ctx):
    """Compile .rda files for the ratchets R package."""
    ctx.run('cd ratchets && Rscript data-raw/use-data.R')
