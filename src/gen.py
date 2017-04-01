import argparse
import os

from arm7tdmi import arm_core_gen, thumb_core_gen

parser = argparse.ArgumentParser(description='Generate sources for CPU cores.')
parser.add_argument('--out-dir', help='set output directory')

args = parser.parse_args()
out_dir = args.out_dir

thumb_core_gen.generate(os.path.join(out_dir, 'thumb_core_generated.rs'))
arm_core_gen.generate(os.path.join(out_dir, 'arm_core_generated.rs'))
