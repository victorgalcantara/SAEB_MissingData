## Análise de dados faltantes no SAEB
# Estrutura da Análise

Analysis of missing data in brazilian national evaluation of basic education (SAEB). It can help to know the quality of databases of SAEB.

Neste arquivo disponibilizamos os procedimentos utilizados para a análise dos dados faltantes ( _missing data_) nas bases de dados da Avaliação Nacional da Educação Básica (SAEB) referente às *escolas*. O SAEB é a principal avaliação nacional da Educação Básica, e as bases de dados sobre as escolas agregam diversas informações, como: a estrutura das escolas, o perfil docente, o perfil da direção,  a proficiência média, o contexto social, econômico e cultural dos estudantes, entre outras. O objetivo desta análise é compreender a falta de informação sobre a proficiência dos estudantes, que é tomada como referência para os estudos em desigualdades de oportunidades educacionais.

A análise está estruturada em três etapas. 

# 1. Etapa

Na primeira, averiguamos a participação das escolas no SAEB tendo como base as escolas cadastradas no censo escolar, que é de preenchimento obrigatório e anual. Tomamos o censo como base do universo das escolas que compõem o sistema e comparamos com as escolas com dados disponibilizados nas bases do SAEB.

O procedimento utilizado é uma simples computação de frequência de casos condicionados por Unidades da Federação e Dependências Administrativas. Com a frequência de escolas no censo e no saeb, calculamos uma taxa de participação das escolas pela divisão da frequência total de escolas no saeb pela frequência total de escolas no censo.

taxa de participação = SAEB / CENSO

Para os anos 2013 e 2015 consideramos apenas escolas que oferecem Ensino Fundamental, uma vez que o SAEB era censitário apenas para este segmento. Para 2017 e 2019 consideramos as escolas que oferecem Ensino Fundamental, Médio tradicional e integrado ao técnico, uma vez que o SAEB passou a ser censitário para o Ensino Médio. Com essa informação podemos ter uma primeira apreensão sobre a participação e representatividade das escolas na avaliação. 

Nesta etapa observamos que, ainda que o SAEB seja obrigatório e censitário desde 2013 para o Ensino Fundamental e desde 2015 para o Ensino Médio, dando continuidade às aplicações censitárias da Prova Brasil entre 2007 e 2011, há fatores que excluem a participação ou apresentação dos resultados de parte significativa das escolas municipais e estaduais. Entre os fatores, há condições técnicas estabelecidas pelo INEP, como a não aplicação da avaliação para escolas com menos de 20 estudantes matriculados e a não divulgação do resultado da avaliação para escolas que não tenham um percentual mínimo 80% de participação dos estudantes. Para escolas municipais, o percentual mínimo é ainda menor, de 50%. São considerados participantes os estudantes presentes na aplicação da avaliação e que estavam declarados no censo escolar do ano de referência. Os participantes compõem o grupo a partir do qual calcula-se os indicadores para as escolas, como a proficiência média e o IDEB.

# 2. Etapa

Na segunda etapa, analisamos a quantidade de casos sem informações entre as escolas que participaram do SAEB. Nesta, averiguamos a condição crítica da informação disponível, com parte significativa de dados faltantes. Verificamos que para escolas Federais a situação é mais grave (com em torno de 90% dos casos sem informação). 

# 3. Etapa

Por fim, fazemos uma síntese da análise, com a representação da informação presente nas bases ao longo do tempo. Concluimos que a falta de informações nas bases torna o diagnóstico menos preciso, uma vez que as informações estão enviesadas por casos de escolas em que os estudantes comparecem no dia da avaliação e se prontificam em realizá-la.

Este projeto está em andamento e aberto para construção conjunta. Se tiver alguma sugestão ou crítica, pode entrar em contato através do meu e-mail pessoal <victoralcantara.csocial@gmail.com>.
Este projeto pode ser aberto, reproduzido e modificado. Peço apenas que referencie meu trabalho em caso de uso para divulgação ou publicação deste conteúdo. Aproveite! ;)

This project is licensed under the terms of the MIT license. Enjoy it! ;)
