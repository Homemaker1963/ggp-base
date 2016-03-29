#!/usr/bin/env python

from __future__ import print_function
import subprocess

def _run(log_folder, run_id, game_name, start_clock, play_clock, p1_name, p1_port, p2_name, p2_port, iterations):
    for i in range(iterations):
        print("Running iteration: %d of %d" % (i, iterations))
        print("==============================================")
        subprocess.check_call(['./gameServerRunner.sh',
                               log_folder,
                               game_name,
                               str(start_clock),
                               str(play_clock),
                               '127.0.0.1',
                               str(p1_port),
                               p1_name + "-" + run_id,
                               '127.0.0.1',
                               str(p2_port),
                               p2_name + "-" + run_id])

def run(log_folder, run_id, game_name, start_clock, play_clock, p1_name, p1_port, p2_name, p2_port, iterations):
    _run(log_folder, run_id, game_name, start_clock, play_clock, p1_name, p1_port, p2_name, p2_port, iterations)
    _run(log_folder, run_id, game_name, start_clock, play_clock, p2_name, p2_port, p1_name, p1_port, iterations)

if __name__ == '__main__':
    run("mast-test", "connectFour-60-30",      "connectFour",      60, 30, "vanilla", 4000, "mast", 5000, iterations=10)
    run("mast-test", "checkers-60-30",         "checkers",         60, 30, "vanilla", 4000, "mast", 5000, iterations=10)
    run("mast-test", "breakthrough-60-30",     "breakthrough",     60, 30, "vanilla", 4000, "mast", 5000, iterations=10)
    run("mast-test", "othello-60-30",          "othello-comp2007", 60, 30, "vanilla", 4000, "mast", 5000, iterations=10)
    run("mast-test", "pentago-60-30",          "pentago",          60, 30, "vanilla", 4000, "mast", 5000, iterations=10)
    run("mast-test", "chinesecheckers2-60-30", "chineseCheckers2", 60, 30, "vanilla", 4000, "mast", 5000, iterations=10)
    run("mast-test", "quarto-60-30",            "quarto",          60, 30, "vanilla", 4000, "mast", 5000, iterations=10)
    run("mast-test", "skirmish-60-30",          "skirmish",        60, 30, "vanilla", 4000, "mast", 5000, iterations=10)
