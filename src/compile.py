import sys
import argparse
import importlib
import inspect

from edg import *

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--module', help='python module containing top-level block')
    parser.add_argument('--block', help='top-level block to be compiled')
    parser.add_argument('--output_dir', help='directory for output .edg files')
    args = parser.parse_args()

    assert args.module and args.block and args.output_dir, "Missing required arguments"

    module_name = args.module.split(".")[0].replace("/", ".").split(".")  # strip potential file extension e.g. '<module_name>.py'
    block_name = args.block
    output_dir = args.output_dir

    module = importlib.import_module(module_name[-1], "".join(module_name[:-1]))
    block = getattr(module, block_name)
    ElectronicsDriver([module]).generate_write_block(
        block(),
        output_dir
    )