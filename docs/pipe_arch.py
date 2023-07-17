import diagrams.programming
from diagrams import Cluster, Diagram, Edge
from diagrams.aws.compute import *
from diagrams.aws.media import *
from diagrams.aws.database import *


with Diagram("pipeline architecture", show=True):
    with Cluster("CU"):
        inst_fetch =  EC2Ami("IF")
        inst_decode = Batch("ID")
        operand_fetch = ECR("OF")
        with Cluster("EXE") as exe:
            alu = EC2Instance("ALU")
            lsu = ElementalMedialive("LSU")

        write_back = ElasticTranscoder("WB")
        inst_fetch >> inst_decode
        inst_decode >> operand_fetch
        operand_fetch >> alu
        lsu >> write_back

        regs = ElasticacheForRedis("Regs")
        regs >> operand_fetch
        write_back >> regs

        score_board = DynamodbTable("ScoreBoard")
        score_board << Edge() >> inst_decode
        score_board << Edge() >> write_back
    ex_mem = ElasticacheForMemcached("External mem")
    ex_mem >> inst_fetch
    lsu << Edge() >> ex_mem
    # write_back >> regs
