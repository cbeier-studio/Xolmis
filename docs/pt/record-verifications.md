# Verificações de registros

O módulo de **Verificações de registros** é usado para acompanhar a **qualidade e precisão dos dados** armazenados no Xolmis. A verificação garante que os registros sejam confiáveis, consistentes e adequados para análise científica. Cada verificação é realizada por um pesquisador e armazenada como uma **trilha de auditoria**, ajudando a manter transparência e responsabilidade em projetos colaborativos.

![Diálogo de verificações de registros](img/record-verifications-dialog.png)

## Adicionando uma verificação

![Novo diálogo de verificação de registro](img/new-record-verification-dialog.png)

Ao criar um registro de verificação, os seguintes campos estão disponíveis:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Status** | Sim | Status geral do registro (ver detalhes abaixo) |
| **Pesquisador** | Sim | Pessoa que revisou e verificou o registro (selecionada na tabela de Pesquisadores) |
| **Notas** |  | Detalhes da revisão ou qualquer outra informação sobre a verificação |

## Opções de status

O campo **status** indica o resultado do processo de verificação. Cada opção ajuda a identificar problemas específicos ou confirmar que o registro está correto:

- **Registro OK** – O registro foi revisado e está correto.  
- **Táxon incorreto** – A identificação taxonômica está incorreta.  
- **Local incorreto** – As informações de localidade estão incorretas.  
- **Coordenadas incorretas** – As coordenadas geográficas estão imprecisas.  
- **Medição incorreta** – Uma medição (ex.: comprimento, massa) está incorreta.  
- **Valor incorreto** – Um valor geral no registro está incorreto.  
- **Dados ausentes** – O registro está incompleto e requer informações adicionais.  
- **Registro duplicado** – O registro é uma duplicata de outra entrada.  
- **Dados inconsistentes** – Os valores do registro entram em conflito entre si ou com referências externas.  
- **Registro suspeito** – O registro parece incomum ou duvidoso, exigindo investigação adicional.  
- **Fora do intervalo** – Uma medição ou valor está fora do intervalo biológico ou lógico esperado.  
- **Registro obsoleto** – O registro está desatualizado ou foi substituído por informações mais recentes.  

## Boas práticas

- **Sempre atribua um pesquisador**: As verificações devem estar vinculadas a um revisor responsável para garantir responsabilidade.  
- **Use notas para contexto**: Documente por que um registro foi marcado ou corrigido para ajudar revisores futuros.  
- **Seja específico com o status**: Escolha a opção de status mais precisa para evitar ambiguidades.  
- **Revise primeiro os campos críticos**: Táxon, localidade e coordenadas são essenciais para análises ecológicas.  
- **Reverifique após correções**: Uma vez atualizado o registro, realize nova verificação para confirmar a precisão.  
- **Mantenha consistência**: Aplique os mesmos padrões de verificação em todos os módulos para garantir integridade dos dados.  

## Relação com outros módulos

As verificações de registros estão interconectadas com várias partes do Xolmis:

- **[Indivíduos](individuals.md)** – Verificar informações de anilhamento, sexo e idade.  
- **[Capturas](captures.md)** – Conferir medições e detalhes de captura.  
- **[Observações](sightings.md)** – Confirmar identificação taxonômica e tipo de detecção.  
- **[Amostragens](surveys.md)** – Validar localidade, coordenadas e métodos de amostragem.  

Ao utilizar as verificações de registros, o Xolmis fornece um **sistema estruturado de controle de qualidade**, garantindo que os dados ornitológicos sejam confiáveis e prontos para pesquisa científica.
