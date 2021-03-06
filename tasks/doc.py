from glob import glob
from invoke import task
from unipath import Path


arg_docs = dict(
    loc=(
        'Directory containing the doc. Can be a glob. '
        'Assumed to be a subdirectory of "docs/".'
    ),
    name=(
        'Doc names to match. Defaults to *.Rmd. Searched '
        'inside of `loc`.'
    ),
    ext=(
        'The extension of the output document. Defaults to "html". '
        'This can also be set in the Rmd front matter. '
        'Determines which file gets opened.'
    ),
    dry_run='Print matching docs without compiling them.',
    reset='Clear cache and figs loc before compiling.',
    open_after='Open the doc after compiling it.',
)


@task(help=arg_docs)
def render(ctx, loc='', name='*.Rmd', dry_run=False, reset=False,
           open_after=False, verbose=False):
    """Render Rmarkdown documents."""
    docs = find_docs(name, loc)

    if dry_run:
        print('\n'.join(docs))
        return

    if reset:
        clear(ctx, loc=loc, name=name, verbose=verbose)

    cmd = 'Rscript -e "rmarkdown::render(\'{doc}\')"'
    for doc in docs:
        ctx.run(cmd.format(doc=doc), echo=verbose)

        if open_after:
            out = Path(doc.parent, doc.stem + '.html')
            ctx.run('open {}'.format(out), echo=verbose)


@task(help=arg_docs)
def clear(ctx, loc='', name='*.Rmd', verbose=False):
    """Clear the cache and figs directories."""
    cmd = 'rm -rf {cache} {figs}'

    docs = find_docs(name, loc)

    for doc in docs:
        ctx.run(cmd.format(cache=Path(doc.parent, '.cache'),
                           figs=Path(doc.parent, 'figs')),
                echo=verbose)




def find_docs(name, loc=None):
    """Return paths to docs in loc matching name."""
    if loc and not Path(loc).isdir():
        # loc refers to glob inside docs loc
        loc = Path('docs/', loc)

    return [Path(doc) for doc in glob(Path(loc, name))]
