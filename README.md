# Simulador de Contagio Financiero  
**Aplicación Shiny basada en el modelo de Gai & Kapadia (2011)**  

> Analiza cómo la estructura de una red interbancaria influye en la propagación del riesgo sistémico y visualiza, paso a paso, los efectos de un shock inicial sobre los bancos.

---

## 📑 Tabla de contenidos
1. [Motivación](#motivación)  
2. [Características principales](#características-principales)  
3. [Demo rápida](#demo-rápida)  
4. [Requisitos](#requisitos)  
5. [Instalación local](#instalación-local)  
6. [Uso de la app](#uso-de-la-app)  
7. [Estructura del repositorio](#estructura-del-repositorio)  
8. [Contribuir](#contribuir)  
9. [Licencia](#licencia)  
10. [Referencias](#referencias)  

---

## Motivación
El colapso de instituciones financieras puede propagarse como un “virus” a través de sus obligaciones cruzadas.  
El modelo de **Gai & Kapadia (2011)** muestra que una red puede ser *robusta pero frágil*: sobrevive a pequeños golpes, pero un shock suficientemente grande desencadena un efecto dominó.  
Esta aplicación convierte ese marco teórico en una simulación interactiva para enseñar —o investigar— riesgo sistémico.

## Características principales
| Módulo | Descripción | Tecnologías |
|--------|-------------|-------------|
| **Configurador de red** | Crea redes *aleatoria*, *escala‑libre*, *core‑periphery*, *small‑world*, etc. Ajusta número de bancos y probabilidad de conexión. | `igraph`, `visNetwork` |
| **Parámetros financieros** | Define ratio de capital, exposición interbancaria y proporción de activos ilíquidos. | Controles `sliderInput` |
| **Motor de shocks** | Aplica shocks aleatorios o dirigidos (a los bancos más grandes o más conectados) con magnitud configurable. | Lógica R |
| **Visualización dinámica** | Grafica la red en tiempo real, líneas de contagio, tablas de bancos sistémicos y *value boxes* de resumen. | `visNetwork`, `plotly`, `shinydashboard` |
| **Análisis de robustez** | Registra múltiples simulaciones y compara topologías en un solo panel. | `ggplot2`, `plotly` |

## Demo rápida
```r
# En R o RStudio
shiny::runApp("app.R")
```

También puedes probarla online aquí: **<https://owxbz2-daniel-otero.shinyapps.io/FinancialRisk/>**.

## Requisitos
- **R ≥ 4.1**  
- Paquetes: `shiny`, `shinydashboard`, `igraph`, `visNetwork`, `plotly`, `dplyr`, `tidyr`, `DT`, `shinycssloaders`, `ggplot2`.

Instálalos de una vez:
```r
install.packages(c(
  "shiny", "shinydashboard", "igraph", "visNetwork",
  "plotly", "dplyr", "tidyr", "DT", "shinycssloaders", "ggplot2"
))
```

## Instalación local
```bash
git clone https://github.com/tuusuario/contagio-financiero.git
cd contagio-financiero
R -e "shiny::runApp('app.R')"
```

## Uso de la app
1. **Configura la red**: selecciona topología, número de bancos y probabilidad de conexión.  
2. **Ajusta parámetros financieros**: ratio de capital, exposición interbancaria y activos ilíquidos.  
3. **Diseña el shock**: magnitud, número de bancos afectados y criterio de selección.  
4. **Ejecuta la simulación** y observa:  
   - Evolución de *bancos seguros, shockeados y quebrados*.  
   - Porcentaje de activos perdidos.  
   - Tabla de bancos sistémicamente importantes.  
5. **Reinicia** y experimenta con distintos escenarios para comparar robustez.

## Estructura del repositorio
```
├── app.R              # Código completo de la aplicación Shiny
├── README.md          # (este archivo)
├── www/               # Recursos estáticos: logos, CSS, capturas
└── LICENCE
```

## Contribuir
¿Tienes ideas para nuevas topologías, métricas o visualizaciones?

1. Haz un fork y crea una rama descriptiva (`feature/core-periphery-weighted`).  
2. Asegúrate de que `lintr` y las pruebas (si las añades) pasen.  
3. Envía un **pull request** con detalles sobre el cambio y la motivación.

## Licencia
Distribuido bajo la **MIT License** — consulta el archivo `LICENCE` para más información.

## Referencias
- **Gai, P., & Kapadia, S.** (2011). *Contagion in Financial Networks*. *Proceedings of the Royal Society A*, 466(2120), 2401‑2423.  
- **LeBaron, B.** *Financial Contagion Model* (adaptación NetLogo).  
- Código de la app inspirado y extendido a partir de implementaciones docentes en R.

---

> Explora la fragilidad oculta en las redes financieras y descubre cómo pequeñas decisiones de estructura pueden amplificar —o mitigar— un colapso sistémico.
