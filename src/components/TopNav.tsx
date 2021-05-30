import { MoonIcon, SunIcon } from "@chakra-ui/icons";
import { Box, Button, useColorMode, useColorModeValue } from "@chakra-ui/react";
import Link from "next/link";
import useScreenWiderThanBreakpoint from "../hooks/useScreenWiderThanBreakpoint";

const TopNav: React.FC<{ className?: string }> = ({ className }) => {
  const { toggleColorMode, colorMode } = useColorMode();
  const backgroundColor = useColorModeValue("gray.100", "gray.900");
  const shadow = useColorModeValue(
    " 1px 0px 6px 1px rgba(0,0,0,0.2)",
    "-1px 0px 4px 1px rgba(255,255,255,0.2)"
  );

  const isWide = useScreenWiderThanBreakpoint(1200);

  return (
    <Box
      className={className}
      backgroundColor={backgroundColor}
      css={{
        width: "100%",
        height: 52,
        flexShrink: 0,
        display: "flex",
        alignItems: "center",
        justifyContent: "space-between",
        boxShadow: shadow,
        zIndex: 98,
      }}
    >
      <Link href="/" passHref>
        <Button
          as="a"
          title="home"
          css={{
            left: 16,
            zIndex: 99,
            padding: 2,
          }}
        >
          <img src="/assets/plutus-logo.svg" />
        </Button>
      </Link>
      <Button
        title="toggle-color-mode"
        css={{
          right: 16,
          zIndex: 99,
          padding: 2,
          borderRadius: "50%",
          boxShadow: "1px 0px 6px 1px rgba(0,0,0,0.2)",
        }}
        onClick={toggleColorMode}
      >
        {colorMode === "dark" ? <MoonIcon /> : <SunIcon color="yellow.500" />}
      </Button>
    </Box>
  );
};

export default TopNav;
