---
description: "Use when adding or editing unit tests under source/tests (FPCUnit, xolmis_test project). Covers test layout, registration, determinism, and how to run."
name: "Xolmis Tests Conventions"
applyTo:
  - "source/tests/**/*.pp"
  - "source/tests/**/*.lpr"
---

# Xolmis Tests Conventions

- Framework: FPCUnit com GUI runner. Projeto: `source/tests/xolmis_test.lpi` / `xolmis_test.lpr`.
- Nome do arquivo `test_<unit_testada>.pp`; uma classe `TTest...` (`TTestCase`) por unit testada, registrada em `initialization` com `RegisterTest`.
- **Toda unit de teste nova deve entrar na cláusula `uses` de `xolmis_test.lpr`** (e no `.lpi` se aplicável), senão não roda.
- Testes determinísticos: sem dependência de rede, relógio, locale da máquina ou do banco real do usuário. Para SQL, use banco SQLite em memória/temporário criado e removido no teste (`setUp`/`tearDown`).
- Priorize lógica pura e testável: utils (conversões, GIS, math, taxonomia), `data_setparam`, IO core, repositórios/providers, mapeamentos XRS (contexto Mustache sem UI).
- Corrija bugs com um teste de regressão sempre que a área tiver cobertura viável.
- Não altere `xolmis_test.exe`, `*.res`, `*.lps`. Antes de concluir, compile e rode o projeto de testes com `lazbuild`; se não for possível rodar a GUI, ao menos compile e informe.
