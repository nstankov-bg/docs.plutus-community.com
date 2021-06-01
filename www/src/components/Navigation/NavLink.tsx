import { useColorModeValue } from "@chakra-ui/color-mode";
import { Link as LinkUI } from "@chakra-ui/layout";
import Link from "next/link";
import { useRouter } from "next/router";

export const NavLink: React.FC<Record<string, any>> = ({ href, ...props }) => {
  const { asPath } = useRouter();
  const color = useColorModeValue("cyan.800", "cyan.200");
  const colorActive = useColorModeValue("cyan.800", "cyan.100");
  const bgColorActive = useColorModeValue("white", "gray.600");

  const isActive = href === asPath;

  return (
    <Link href={href} passHref>
      <LinkUI
        {...props}
        display="block"
        bgColor={isActive ? bgColorActive : "transparent"}
        color={isActive ? colorActive : color}
        mb="2"
        p="1"
        pl="2"
        borderRadius="sm"
        fontWeight={isActive ? "bold" : "normal"}
        css={{
          boxShadow: isActive ? "1px 0px 4px 1px rgba(0,0,0,0.1)" : "none",
        }}
      />
    </Link>
  );
};
