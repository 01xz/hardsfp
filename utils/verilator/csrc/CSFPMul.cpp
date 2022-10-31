#include <verilated.h>
#include <verilated_vcd_c.h>

#include <cassert>
#include <cstdio>

#include "sfp.hh"
#include "VSFPMul.h"

int main(int argc, char* argv[], char* env[]) {
  if (false && argc && argv && env) {
  }

  VerilatedVcdC* tfp = new VerilatedVcdC;

  VerilatedContext* contextp = new VerilatedContext;
  contextp->traceEverOn(true);
  VSFPMul* top = new VSFPMul{contextp};

  top->trace(tfp, 0);
  tfp->open("build/SFPMul.vcd");

  auto a = SFP(3, 3);
  auto b = SFP(3, 3);
  auto result = SFP(4, 4);
  for (unsigned i = 0; i < (unsigned)(1 << a.nBits()); i++) {
    for (unsigned j = 0; j < (unsigned)(1 << a.nBits()); j++) {
      if (!contextp->gotFinish()) {
        top->clk = 0;
        a.setBits(i);
        b.setBits(j);
        top->a = i;
        top->b = j;
        top->eval();
        tfp->dump(contextp->time());
        contextp->timeInc(1);

        top->clk = (top->clk == 1 ? 0 : 1);
        top->eval();
        tfp->dump(contextp->time());
        contextp->timeInc(1);

        result.setBits(top->result);

        printf("=====================================\n");
        printf("a:\n");
        a.print();
        printf("b:\n");
        b.print();
        printf("result:\n");
        result.print();
        printf("ref result:\n");
        (a * b).print();

        assert(result == (a * b));
      }
    }
  }

  tfp->close();

  top->final();

  delete top;
  delete contextp;

  return 0;
}
