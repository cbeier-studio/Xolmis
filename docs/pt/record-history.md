# Histórico do registro

O módulo de **Histórico do registro** fornece um **log somente leitura** de todas as alterações feitas em um registro. É uma ferramenta essencial para **auditoria, rastreabilidade e responsabilidade**, garantindo que cada modificação possa ser acompanhada ao longo do tempo. Os usuários não podem editar o histórico diretamente — ele é gerado automaticamente pelo sistema sempre que um registro é criado, modificado, excluído ou restaurado.

![Diálogo de histórico do registro](img/record-history-dialog.png)

## Campos

Cada entrada no histórico de registros contém as seguintes informações:

| Campo | Descrição |
| --- | --- |
| **Data** | Data e hora do evento |
| **Ação** | Tipo de ação realizada (ver detalhes abaixo) |
| **Propriedade** | Campo ou propriedade que foi editada |
| **Valor antigo** | Valor da propriedade antes da edição |
| **Valor novo** | Valor da propriedade após a edição |
| **Notas** | Informações adicionais sobre o evento (opcional) |
| **Usuário** | Usuário que realizou a ação |

## Ações

As seguintes ações são registradas no histórico de registros:

- **Inserir** – Um novo registro foi criado.  
- **Editar** – Um registro existente foi modificado.  
- **Excluir** – Um registro foi removido (enviado para a lixeira).  
- **Restaurar** – Um registro previamente excluído foi restaurado.  

## Boas práticas

- **Revise o histórico antes de editar**: Verifique alterações anteriores para entender o contexto de um registro.  
- **Audite regularmente**: Revise periodicamente os históricos para garantir integridade dos dados e conformidade.  
- **Aproveite os nomes de usuário**: Acompanhe quem fez as alterações para garantir responsabilidade em ambientes colaborativos.  
- **Restaure com cuidado**: Use a ação de restauração apenas quando tiver certeza de que o registro deve ser reativado.  

Ao manter um histórico detalhado de registros, o Xolmis garante **confiabilidade e transparência dos dados**, apoiando tanto a pesquisa científica quanto a gestão administrativa.
