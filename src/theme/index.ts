import { extendTheme, ThemeConfig } from "@chakra-ui/react";

export const fonts = {
  heading: "Noto Sans TC, sans-serif",
  mono: "Fira Mono, monospace",
};

export const config: ThemeConfig = {
  initialColorMode: 'dark',
};

export default extendTheme({ fonts, config });
